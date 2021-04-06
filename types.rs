use std::cell::RefCell;
use std::rc::Rc;
use itertools::Itertools;

use colored::*;

use fnv::FnvHashMap;
use num_bigint::{BigInt, Sign};

use crate::types::MalVal::{Atom, Bool, Func, Int, List, MalFunc, Nil, Str, Sym, Vector, Hash};
use crate::types::MalErr::{ErrString, ErrMalVal};
use crate::env::{Env, env_bind};

#[derive(Debug, Clone)]
pub enum MalVal {
    Nil,
    Bool(bool),
    Int(BigInt),
    //Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalVal>>, Rc<MalVal>),
    Vector(Rc<Vec<MalVal>>, Rc<MalVal>),
    Hash(Rc<FnvHashMap<String, MalVal>>, Rc<MalVal>),
    Func(fn(MalArgs) -> MalRet, Rc<MalVal>),
    MalFunc {
        eval: fn(ast: MalVal, env: Env) -> MalRet,
        ast: Rc<MalVal>,
        env: Env,
        params: Rc<MalVal>,
        is_macro: bool,
        meta: Rc<MalVal>,
    },
    Atom(Rc<RefCell<MalVal>>),
}




#[derive(Debug)]
pub enum MalErr {
    ErrString(String),
    ErrMalVal(MalVal),
}

pub type MalArgs = Vec<MalVal>;
pub type MalRet = Result<MalVal, MalErr>;

macro_rules! list {
  ($seq:expr) => {{
    List(Rc::new($seq),Rc::new(Nil))
  }};
  [$($args:expr),*] => {{
    let v: Vec<MalVal> = vec![$($args),*];
    List(Rc::new(v),Rc::new(Nil))
  }}
}

macro_rules! vector {
  ($seq:expr) => {{
    Vector(Rc::new($seq),Rc::new(Nil))
  }};
  [$($args:expr),*] => {{
    let v: Vec<MalVal> = vec![$($args),*];
    Vector(Rc::new(v),Rc::new(Nil))
  }}
}

pub fn error(s: &str) -> MalRet {
    Err(ErrString(s.to_string()))
}

pub fn format_error(e: MalErr) -> String {
    match e {
        ErrString(s) => format!("{}: {}", "Error".red(), s.clone()),
        ErrMalVal(v) => v.pr_str(true),
    }
}

pub fn func(f: fn(MalArgs) -> MalRet) -> MalVal {
    Func(f, Rc::new(Nil))
}

pub fn atom(mv: &MalVal) -> MalVal{
    Atom(Rc::new(RefCell::new(mv.clone())))
}

pub fn int_to_bigint(i: i64) -> BigInt {
    if i > 0 {
        BigInt::new(Sign::Plus, vec![i as u32])
    }
    else if i < 0 {
        BigInt::new(Sign::Minus, vec![(-i) as u32])
    }
    else {
        BigInt::new(Sign::NoSign, vec![])
    }
}


impl MalVal {
    pub fn keyword(&self) -> MalRet {
        match self {
            Str(s) if s.starts_with("\u{29e}") => Ok(Str(s.to_string())),
            Str(s) => Ok(Str(format!("\u{29e}{}", s))),
            _ => error("invalid type for keyword"),
        }
    }

    pub fn keyword_q(&self) -> bool {
        match self {
            Str(s) if s.starts_with("\u{29e}") => true,
            _ => false,
        }
    }

    pub fn apply(&self, args: MalArgs) -> MalRet {
        match *self {
            Func(f, _) => f(args),
            MalFunc {
                eval, 
                ref ast, 
                ref env, 
                ref params,
                ..
            } => {
                let a = &**ast;
                let fn_env = env_bind(Some(env.clone()), (**params).clone(), args)?;
                eval(a.clone(), fn_env)
            },
            _ => error("attempt to call non-function"),
        }
    }

    pub fn empty_q(&self) -> MalRet {
        match self {
            List(l, _) | Vector(l, _) => Ok(Bool(l.len() == 0)),
            Str(s) => Ok(Bool(s.len() == 0)),
            Nil => Ok(Bool(true)),
            _ => error("invalid type for empty?"),
        }
    }

    pub fn count(&self) -> MalRet {
        match self {
            List(ls,  _) | Vector(ls, _) => Ok(Int(BigInt::new(Sign::Plus, vec![ls.len() as u32]))),
            Nil => Ok(Int(BigInt::new(Sign::NoSign, vec![0]))),
            _ => error("invalid type for count"),
        }
    }

    pub fn deref(&self) -> MalRet {
        match self {
            Atom(a) => Ok(a.borrow().clone()),
            _ => error("attempt to deref a non-Atom"),
        }
    }

    pub fn reset_bang(&self, new: &MalVal) -> MalRet {
        match self {
            Atom(a) => {
                *a.borrow_mut() = new.clone();
                Ok(new.clone())
            }
            _ => error("attempt to reset! a non-Atom"),
        }
    }

    pub fn swap_bang(&self, args: &MalArgs) -> MalRet {
        match self {
            Atom(a) => {
                let f = &args[0];
                let mut fargs = args[1..].to_vec();
                fargs.insert(0, a.borrow().clone());
                *a.borrow_mut() = f.apply(fargs)?;
                Ok(a.borrow().clone())
            }
            _ => error("attempt to swap! a non-Atom"),
        }
    }

    pub fn get_meta(&self) -> MalRet {
        match self {
            List(_, meta) | Vector(_, meta) | Hash(_, meta) => Ok((&**meta).clone()),
            Func(_, meta) => Ok((&**meta).clone()),
            MalFunc { meta, .. } => Ok((&**meta).clone()),
            _ => error("meta not supported by type"),
        }
    }

    pub fn with_meta(&mut self, new_meta: &MalVal) -> MalRet {
        match self {
            List(_, ref mut meta)
            | Vector(_, ref mut meta)
            | Hash(_, ref mut meta)
            | Func(_, ref mut meta)
            | MalFunc { ref mut meta, .. } => {
                *meta = Rc::new((&*new_meta).clone());
            }
            _ => return error("with-meta not supported by type"),
        };
        Ok(self.clone())
    }

    pub fn type_info(&self) -> String {
        match self {
            Nil => "nil".to_string(),
            Bool(_) => "bool".to_string(),
            Int(_) => "int".to_string(),
            Str(_) => "str".to_string(),
            Sym(_) => "sym".to_string(),
            List(_,_) => "list".to_string(),
            Vector(_,_) => "vector".to_string(),
            Hash(_,_) => "hash".to_string(),
            Func(_,_) => "func".to_string(),
            MalFunc {..} => "mal_func".to_string(),
            Atom(_) => "atom".to_string(),
        }
    }
}

impl PartialEq for MalVal {
    fn eq(&self, other: &MalVal) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(ref a), Bool(ref b)) => a == b,
            (Int(ref a), Int(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Sym(ref a), Sym(ref b)) => a == b,
            (List(ref a, _), List(ref b, _))
            | (Vector(ref a, _), Vector(ref b, _))
            | (List(ref a, _), Vector(ref b, _))
            | (Vector(ref a, _), List(ref b, _)) => a == b,
            (Hash(ref a, _), Hash(ref b, _)) => a == b,
            (MalFunc { .. }, MalFunc { .. }) => false,
            _ => false,
        }
    }
}

pub fn _assoc(mut hm: FnvHashMap<String, MalVal>, kvs: MalArgs) -> MalRet {
    if kvs.len() % 2 != 0 {
        return error("odd number of elements");
    }
    for (k, v) in kvs.iter().tuples() {
        match k {
            Str(s) => {
                hm.insert(s.to_string(), v.clone());
            }
            _ => return error("key is not string"),
        }
    }
    Ok(Hash(Rc::new(hm), Rc::new(Nil)))
}

pub fn _dissoc(mut hm: FnvHashMap<String, MalVal>, ks: MalArgs) -> MalRet {
    for k in ks.iter() {
        match k {
            Str(ref s) => {
                hm.remove(s);
            }
            _ => {
                println!("key is not string");
                return error("key is not string");
            }
        }
    }
    Ok(Hash(Rc::new(hm), Rc::new(Nil)))
}

pub fn hash_map(kvs: MalArgs) -> MalRet {
    let hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
    _assoc(hm, kvs)
}

