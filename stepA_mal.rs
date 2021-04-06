use std::rc::Rc;
use fnv::FnvHashMap;
use itertools::Itertools;
use std::net::UdpSocket;

extern crate rustyline;
extern crate itertools;

use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
mod types;
mod reader;
mod printer;
mod env;
mod core;

use crate::types::{MalVal, MalArgs, MalRet, MalErr};
use crate::types::MalErr::{ErrString, ErrMalVal};
use crate::types::MalVal::{Bool, Func, List, MalFunc, Nil, Sym, Str, Vector, Hash};
use crate::types::{error, format_error};
use crate::env::{env_new, env_bind, env_set, env_sets, env_get, Env};

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
        acc = list![Sym("cons".to_string()), quasiquote(&elt), acc];
    }
    return acc;
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
            return qq_iter(&v);
        },
        Vector(v, _) => return list![Sym("vec".to_string()), qq_iter(&v)],
        Hash(_, _) | Sym(_)=> return list![Sym("quote".to_string()), ast.clone()],
        _ => ast.clone(),
    }
}

fn is_macro_call(ast: &MalVal, env: Env) -> Option<(MalVal, MalArgs)> {
    match ast {
        List(ls, _) => match env_get(&env, &ls[0]) {
            Ok(f @ MalFunc {is_macro: true, .. }) => Some((f, ls[1..].to_vec())),
            _ => None,
        }
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
        Sym(s) => Ok(env_get(&env, &Sym(s.clone()))?),
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
            Ok(Hash(Rc::new(hm), Rc::new(Nil)))

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
                        env_set(&env, l[1].clone(), eval(l[2].clone(), env.clone())?)
                    }
                    Sym(ref a0) if a0 == "defmacro!" => {
                        match eval(l[2].clone(), env.clone())? {
                            MalFunc {
                                eval,
                                ast,
                                env,
                                params,
                                ..
                            } => {
                                let f = MalFunc{
                                    eval: eval,
                                    ast: ast.clone(),
                                    env: env.clone(),
                                    params: params,
                                    is_macro: true,
                                    meta: Rc::new(Nil),
                                };
                                env_set(&env, l[1].clone(), f)
                            },
                            _ => error("defmacro on non-function"),
                        }

                    }
                    Sym(ref a0) if a0 == "let*" => {
                        env = env_new(Some(env.clone()));
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        match a1 {
                            List(ref binds, _) | Vector(ref binds, _) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        Sym(_) => {
                                            let _ = env_set(
                                                &env, 
                                                b.clone(), 
                                                eval(e.clone(), env.clone())?
                                            );
                                        }
                                        _ => {
                                            return error("let* with non-Sym binding");
                                        }
                                    }
                                }
                            },
                            _ => {
                                return error("let* with non-List bindings");
                            },
                        };
                        ast = a2;
                        continue 'tco;
                    },
                    Sym(ref a0sym) if a0sym == "quote" => Ok(l[1].clone()),
                    Sym(ref a0sym) if a0sym == "quasiquoteexpand" => Ok(quasiquote(&l[1])),
                    Sym(ref a0sym) if a0sym == "quasiquote" => {
                        ast = quasiquote(&l[1]);
                        continue 'tco;
                    },
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
                                    let catch_env = env_bind(
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
                    Sym(ref a0) if a0 == "do" => {
                        match eval_ast(&list!(l[1..].to_vec()), &env)? {
                            List(_, _) => {
                                ast = l.last().unwrap_or(&Nil).clone();
                                continue 'tco;
                            }
                            _ => error("invalid do form"),
                        }
                    },
                    Sym(ref a0) if a0 == "if" => {
                        let cond = eval(l[1].clone(), env.clone())?;
                        match cond {
                            Bool(false) | Nil if l.len() >= 4 => {
                                ast = l[3].clone();
                                continue 'tco;
                            },
                            Bool(false) | Nil => {
                                Ok(Nil)
                            },
                            _ if l.len() >= 3=> {
                                ast = l[2].clone();
                                continue 'tco;
                            },
                            _ => Ok(Nil)
                        }
                    },
                    Sym(ref a0) if a0 == "fn*" => {
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        Ok(MalFunc {
                            eval: eval,
                            ast: Rc::new(a2),
                            env: env.clone(),
                            params: Rc::new(a1),
                            is_macro: false,
                            meta: Rc::new(Nil),
                        })
                    }
                    Sym(ref a0sym) if a0sym == "eval" => {
                        ast = eval(l[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().outer {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    _ => {
                        match eval_ast(&ast, &env)? {
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
                                        env = env_bind(Some(menv.clone()), p.clone(), args)?;
                                        ast = a.clone();
                                        continue 'tco;
                                    }
                                    _ => error("attempt to call non-function"),
                                }
                            }
                            _ => error("Expected list")
                        }
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
    reader::read_str(str.to_string())
}

fn print(ast: &MalVal) -> String {
    ast.pr_str(true)
}

fn rep(line: String, env: &Env) -> Result<String, MalErr> {
    let ast = match read(&line) {
        Ok(a) => a,
        Err(err) => return Err(err),
    };
    let exp = eval(ast, env.clone())?;
    Ok(print(&exp))
}

fn main() {

    let repl_env = env_new(None);
    for (k, v) in core::ns() {
        env_sets(&repl_env, k, v);
    }

    let mut args = std::env::args();
    let arg1 = args.nth(1);
    let arg2 = args.nth(0);
    let arg3 = args.nth(0);
    env_sets(&repl_env, "*ARGV*", list!(args.map(Str).collect()));

    let _= rep("(def! not (fn* (a) (if a false true)))".to_string(), &repl_env);
    let _ = rep(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))".to_string(),
        &repl_env,
    );

    let _ = rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))".to_string(), &repl_env);


    let _ = rep("(def! *host-language* \"flang\")".to_string(), &repl_env);
    
    let mut cmd: String = "".to_string();
    // Invoked with arguments
    
    let input = arg2.clone();
    if let Some(f) = arg1 {
        cmd = match &f[..] {
            "-i" => input.unwrap(),
            "-d" => {
                "daemon".to_string()
            }
            _ => format!("(load-file \"{}\")", f),
        };
        
        if cmd != "daemon" {
            match rep(cmd, &repl_env) {
                Ok(out) => {
                    println!("{}", out);
                    std::process::exit(0);
                }
                Err(err) => {
                    println!("Error: {}", format_error(err));
                    std::process::exit(1);
                }
            }
        }
    }
    
    let port = arg2.clone();
    if cmd == "daemon" {
        println!("daemon");
        let adr = "127.0.0.1:".to_string() + &port.clone().unwrap();
        println!("address: {}", adr);
        let mut socket = match UdpSocket::bind(adr) {
            Ok(socket) => socket,
            Err(err) => {
                println!("Error: {}", err);
                std::process::exit(1);
            }
        };
        loop {
            let mut buf = [0; 1024];
            let (amt, src) = socket.recv_from(&mut buf).unwrap();
            let buf = &mut buf[..amt];
            let out = match rep(String::from_utf8(buf.to_vec()).unwrap(), &repl_env) {
                Ok(out) => {
                    println!("{}", out); 
                    out
                },
                Err(err) => format!("Error: {}", format_error(err))
            };
            socket.send_to(out.as_bytes(), &src);
        }
    }

    // main repl loop
    let _ = rep("(println (str \"Mal [\" *host-language* \"]\"))".to_string(), &repl_env);

    let mut rl = Editor::<()>::new();
    if rl.load_history(".flang-history").is_err() {
        //println!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                rl.save_history(".flang-history").unwrap();
                match rep(line, &repl_env) {
                    Ok(out) => println!("{}", out),
                    Err(err) => println!("Error: {}", format_error(err)),
                }
            },

            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}
