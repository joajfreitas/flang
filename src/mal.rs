use fnv::FnvHashMap;
use itertools::Itertools;
use std::sync::Arc;

use crate::env::Env;
use crate::reader::read_str;
use crate::types::error;
use crate::types::MalErr::{ErrMalVal, ErrString};
use crate::types::MalVal::{Bool, Func, Hash, List, MalFunc, Nil, Str, Sym, Vector};
use crate::types::{MalArgs, MalErr, MalRet, MalVal};

macro_rules! list {
  ($seq:expr) => {{
    List(Arc::new($seq),Arc::new(Nil))
  }};
  [$($args:expr),*] => {{
    let v: Vec<MalVal> = vec![$($args),*];
    List(Arc::new(v),Arc::new(Nil))
  }}
}

macro_rules! vector {
  ($seq:expr) => {{
    Vector(Arc::new($seq),Arc::new(Nil))
  }};
  [$($args:expr),*] => {{
    let v: Vec<MalVal> = vec![$($args),*];
    Vector(Arc::new(v),Arc::new(Nil))
  }}
}

fn qq_iter(elts: &MalArgs) -> MalVal {
    let mut acc = list![];
    for elt in elts.iter().rev() {
        if let List(v, _) = elt {
            if v.len() == 2 {
                if let Sym(ref s) = v[0] {
                    if s == "splice-unquote" {
                        acc = list![Sym("concat".to_string()), v[1].clone(), acc];
                        continue;
                    }
                }
            }
        }
        acc = list![Sym("cons".to_string()), quasiquote(elt), acc];
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
        Vector(v, _) => list![Sym("vec".to_string()), qq_iter(v)],
        Hash(_, _) | Sym(_) => list![Sym("quote".to_string()), ast.clone()],
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
            Ok(list!(lst))
        }
        Vector(v, _) => {
            let mut lst: Vec<MalVal> = vec![];
            for e in v.iter() {
                lst.push(eval(e.clone(), env.clone())?);
            }
            Ok(vector!(lst))
        }
        Hash(h, _) => {
            let mut hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
            for (k, v) in h.iter() {
                hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
            }
            Ok(Hash(Arc::new(hm), Arc::new(Nil)))
        }
        _ => Ok(ast.clone()),
    }
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
                    Sym(ref a0) if a0 == "def!" => {
                        env.set(l[1].clone(), eval(l[2].clone(), env.clone())?)
                    }
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
                                is_macro: true,
                                meta: Arc::new(Nil),
                            };
                            env.set(l[1].clone(), f)
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
                                            let _ =
                                                env.set(b.clone(), eval(e.clone(), env.clone())?);
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
                                        list!(vec![c[1].clone()]),
                                        vec![exc],
                                    )?;
                                    eval(c[2].clone(), catch_env)
                                }
                                _ => error("invalid catch block"),
                            }
                        }
                        res => res,
                    },
                    Sym(ref a0) if a0 == "do" => match eval_ast(&list!(l[1..].to_vec()), &env)? {
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
                        Ok(MalFunc {
                            eval,
                            ast: Arc::new(a2),
                            env: env.clone(),
                            params: Arc::new(a1),
                            is_macro: false,
                            meta: Arc::new(Nil),
                        })
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
                                    ..
                                } => {
                                    let a = (*mast).clone();
                                    let p = (*params).clone();
                                    env = Env::bind(Some(menv.clone()), p.clone(), args)?;
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
