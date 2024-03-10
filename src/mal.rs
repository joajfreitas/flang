use itertools::Itertools;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use crate::env::Env;
use crate::reader::read_str;
use crate::types::error;
use crate::types::MalErr::{ErrMalVal, ErrString};
use crate::types::MalVal::{Bool, Func, Hash, List, MalFunc, Nil, Str, Sym, Vector};
use crate::types::{MalArgs, MalErr, MalRet, MalVal};

fn qq_iter(elts: &MalArgs) -> MalVal {
    let mut acc = MalVal::list(&[]);
    for elt in elts.iter().rev() {
        if let List(v, _) = elt {
            if v.len() == 2 {
                if let Sym(ref s) = v[0] {
                    if s == "splice-unquote" {
                        acc = MalVal::list(&[Sym("concat".to_string()), v[1].clone(), acc]);
                        continue;
                    }
                }
            }
        }
        acc = MalVal::list(&[Sym("cons".to_string()), quasiquote(elt), acc]);
    }
    acc
}

fn quasiquote(ast: &MalVal) -> MalVal {
    match ast {
        List(v, _) => {
            if v.len() == 2 {
                if let Sym(ref s) = v[0] {
                    if s == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            qq_iter(v)
        }
        Vector(v, _) => MalVal::list(&[Sym("vec".to_string()), qq_iter(v)]),
        Hash(_, _) | Sym(_) => MalVal::list(&[Sym("quote".to_string()), ast.clone()]),
        _ => ast.clone(),
    }
}

fn is_macro_call(ast: &MalVal, env: Env) -> Option<(MalVal, MalArgs)> {
    match ast {
        List(ls, _) => match Env::get(&env, &ls[0]) {
            Ok(f @ MalFunc { is_macro: true, .. }) => Some((f, ls[1..].to_vec())),
            _ => None,
        },
        _ => None,
    }
}

fn macroexpand(mut ast: MalVal, env: &Env) -> (bool, MalRet) {
    let mut was_expanded = false;
    while let Some((f, args)) = is_macro_call(&ast, env.clone()) {
        ast = match f.apply(args) {
            Err(e) => return (false, Err(e)),
            Ok(a) => a,
        };

        was_expanded = true;
    }
    (was_expanded, Ok(ast))
}

fn eval_ast(ast: &MalVal, env: &Env) -> MalRet {
    match ast {
        Sym(s) => Ok(env.get(&Sym(s.clone()))?),
        List(l, _) => {
            let mut lst: Vec<MalVal> = vec![];
            for e in l.iter() {
                lst.push(eval(e.clone(), env.clone())?);
            }
            Ok(MalVal::list(&lst))
        }
        Vector(v, _) => {
            let mut lst: Vec<MalVal> = vec![];
            for e in v.iter() {
                lst.push(eval(e.clone(), env.clone())?);
            }
            Ok(MalVal::vector(&lst))
        }
        Hash(h, _) => {
            let mut hm: HashMap<String, MalVal> = HashMap::default();
            for (k, v) in h.iter() {
                hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
            }
            Ok(MalVal::hash(&hm))
        }
        _ => Ok(ast.clone()),
    }
}

#[derive(PartialEq, Eq)]
enum ParamParsing {
    Param,
    Rest,
    Keyword,
}

fn eval_fn(a1: MalVal, a2: MalVal, env: &Env) -> MalRet {
    let params: &Vec<MalVal> = match &a1 {
        MalVal::Vector(params, _) | MalVal::List(params, _) => Ok(params),
        _ => Err(MalErr::ErrString(
            "expected param list after fn*".to_string(),
        )),
    }?;

    let mut ps: VecDeque<String> = VecDeque::new();
    for param in params {
        match param {
            Sym(s) => {
                ps.push_back(s.to_string());
            }
            _ => {
                return Err(MalErr::ErrString(
                    "expected symbol in param list".to_string(),
                ));
            }
        }
    }

    let mut params: Vec<MalVal> = Vec::new();
    let mut rest_params: MalVal = MalVal::Nil;
    let mut keyword_params: Vec<MalVal> = Vec::new();

    let mut parsing_step = ParamParsing::Param;
    loop {
        if ps.is_empty() {
            break;
        }

        let param = ps.pop_front().unwrap();

        if parsing_step == ParamParsing::Param && param == "&rest" {
            parsing_step = ParamParsing::Rest;
            continue;
        } else if (parsing_step == ParamParsing::Rest || parsing_step == ParamParsing::Param)
            && param == "&key"
        {
            parsing_step = ParamParsing::Keyword;
            continue;
        }

        if parsing_step == ParamParsing::Param {
            params.push(MalVal::Sym(param));
        } else if parsing_step == ParamParsing::Rest {
            rest_params = MalVal::Sym(param);
        } else if parsing_step == ParamParsing::Keyword {
            keyword_params.push(MalVal::Sym(param));
        }
    }

    Ok(MalFunc {
        eval,
        ast: Arc::new(a2),
        env: env.clone(),
        params: Arc::new(MalVal::List(Arc::new(params), Arc::new(MalVal::Nil))),
        keyword_params: Arc::new(MalVal::List(
            Arc::new(keyword_params),
            Arc::new(MalVal::Nil),
        )),
        rest_params: Arc::new(rest_params),
        is_macro: false,
        meta: Arc::new(Nil),
    })
}

fn eval(mut ast: MalVal, mut env: Env) -> MalRet {
    let ret: MalRet;

    'tco: loop {
        ret = match ast.clone() {
            List(l, _) => {
                if l.len() == 0 {
                    return Ok(ast);
                }
                match macroexpand(ast.clone(), &env) {
                    (true, Ok(new_ast)) => {
                        ast = new_ast;
                        continue 'tco;
                    }
                    (_, Err(e)) => return Err(e),
                    _ => (),
                }

                match &l[0] {
                    Sym(ref a0) if a0 == "def!" => env.set(&l[1], eval(l[2].clone(), env.clone())?),
                    Sym(ref a0) if a0 == "defmacro!" => match eval(l[2].clone(), env.clone())? {
                        MalFunc {
                            eval,
                            ast,
                            env,
                            params,
                            ..
                        } => {
                            let f = MalFunc {
                                eval,
                                ast: ast.clone(),
                                env: env.clone(),
                                params,
                                keyword_params: Arc::new(MalVal::Nil),
                                rest_params: Arc::new(MalVal::Nil),
                                is_macro: true,
                                meta: Arc::new(Nil),
                            };
                            env.set(&l[1], f)
                        }
                        _ => error("defmacro on non-function"),
                    },
                    Sym(ref a0) if a0 == "let*" => {
                        env = Env::new(Some(env.clone()));
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        match a1 {
                            List(ref binds, _) | Vector(ref binds, _) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        Sym(_) => {
                                            let _ = env.set(b, eval(e.clone(), env.clone())?);
                                        }
                                        _ => {
                                            return error("let* with non-Sym binding");
                                        }
                                    }
                                }
                            }
                            _ => {
                                return error("let* with non-List bindings");
                            }
                        };
                        ast = a2;
                        continue 'tco;
                    }
                    Sym(ref a0sym) if a0sym == "quote" => Ok(l[1].clone()),
                    Sym(ref a0sym) if a0sym == "quasiquoteexpand" => Ok(quasiquote(&l[1])),
                    Sym(ref a0sym) if a0sym == "quasiquote" => {
                        ast = quasiquote(&l[1]);
                        continue 'tco;
                    }
                    Sym(ref a0sym) if a0sym == "macroexpand" => {
                        match macroexpand(l[1].clone(), &env) {
                            (_, Ok(new_ast)) => Ok(new_ast),
                            (_, e) => return e,
                        }
                    }
                    Sym(ref a0sym) if a0sym == "try*" => match eval(l[1].clone(), env.clone()) {
                        Err(ref e) if l.len() >= 3 => {
                            let exc = match e {
                                ErrMalVal(mv) => mv.clone(),
                                ErrString(s) => Str(s.to_string()),
                            };
                            match l[2].clone() {
                                List(c, _) => {
                                    let catch_env = Env::bind(
                                        Some(env.clone()),
                                        MalVal::list(&[c[1].clone()]),
                                        vec![exc],
                                    )?;
                                    eval(c[2].clone(), catch_env)
                                }
                                _ => error("invalid catch block"),
                            }
                        }
                        res => res,
                    },
                    Sym(ref a0) if a0 == "do" => match eval_ast(&MalVal::list(&l[1..]), &env)? {
                        List(r, _) => {
                            ast = r.last().unwrap_or(&Nil).clone();
                            continue 'tco;
                        }
                        _ => error("invalid do form"),
                    },
                    Sym(ref a0) if a0 == "if" => {
                        let cond = eval(l[1].clone(), env.clone())?;
                        match cond {
                            Bool(false) | Nil if l.len() >= 4 => {
                                ast = l[3].clone();
                                continue 'tco;
                            }
                            Bool(false) | Nil => Ok(Nil),
                            _ if l.len() >= 3 => {
                                ast = l[2].clone();
                                continue 'tco;
                            }
                            _ => Ok(Nil),
                        }
                    }
                    Sym(ref a0) if a0 == "fn*" => {
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        eval_fn(a1, a2, &env)
                    }
                    Sym(ref a0sym) if a0sym == "eval" => {
                        ast = eval(l[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().outer() {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    _ => match eval_ast(&ast, &env)? {
                        List(el, _) => {
                            let f = el[0].clone();
                            let args = el[1..].to_vec();
                            match f {
                                Func(_, _) => f.apply(args),
                                MalFunc {
                                    ast: mast,
                                    env: menv,
                                    params,
                                    rest_params,
                                    ..
                                } => {
                                    let p = (*params).clone();
                                    let params: Vec<MalVal> = match &p {
                                        Vector(ps, _) | List(ps, _) => ps.to_vec(),
                                        _ => panic!(),
                                    };
                                    let a = (*mast).clone();
                                    let positional_args = args[0..params.len()].to_vec();
                                    let rest_args = MalVal::list(&args[params.len()..]);
                                    env =
                                        Env::bind(Some(menv.clone()), p.clone(), positional_args)?;

                                    if *rest_params != MalVal::Nil {
                                        env.set(&rest_params, rest_args)?;
                                    }

                                    let kws = args[params.len()..]
                                        .chunks(2)
                                        .filter(|xs| match &xs[0] {
                                            MalVal::Str(s) => s.starts_with('\u{29e}'),
                                            _ => false,
                                        })
                                        .map(|xs| {
                                            (
                                                match &xs[0] {
                                                    MalVal::Str(s) => s,
                                                    _ => panic!(),
                                                },
                                                &xs[1],
                                            )
                                        })
                                        .collect::<Vec<(&String, &MalVal)>>();

                                    for (key, value) in kws {
                                        env.set(
                                            &MalVal::Sym(
                                                key.chars().collect::<Vec<char>>()[1..]
                                                    .iter()
                                                    .collect::<String>(),
                                            ),
                                            value.clone(),
                                        )?;
                                    }
                                    ast = a.clone();
                                    continue 'tco;
                                }
                                _ => error("attempt to call non-function"),
                            }
                        }
                        _ => error("Expected list"),
                    },
                }
            }
            _ => eval_ast(&ast, &env),
        };
        break;
    }
    ret
}

// read
fn read(str: &str) -> MalRet {
    read_str(str.to_string())
}

fn print(ast: &MalVal) -> String {
    ast.pr_str(false)
}

pub fn rep(line: &str, env: &Env) -> Result<String, MalErr> {
    let ast = match read(line) {
        Ok(a) => a,
        Err(err) => return Err(err),
    };
    let exp = eval(ast, env.clone())?;
    let p = print(&exp);
    Ok(p)
}
