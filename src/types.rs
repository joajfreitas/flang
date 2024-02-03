use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::RwLock;

use crate::env::Env;
use crate::types::MalErr::ErrString;
use crate::types::MalVal::{Atom, Bool, Func, Hash, Int, List, MalFunc, Nil, Str, Sym, Vector};

#[derive(Debug, Clone)]
pub enum MalVal {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalVal>>, Rc<MalVal>),
    Vector(Rc<Vec<MalVal>>, Rc<MalVal>),
    Hash(Rc<HashMap<String, MalVal>>, Rc<MalVal>),
    Func(fn(MalArgs) -> MalRet, Rc<MalVal>),
    MalFunc {
        eval: fn(ast: MalVal, env: Env) -> MalRet,
        ast: Rc<MalVal>,
        env: Env,
        params: Rc<MalVal>,
        is_macro: bool,
        meta: Rc<MalVal>,
    },
    Atom(Rc<RwLock<MalVal>>),
}

#[derive(Debug)]
pub enum MalErr {
    ErrString(String),
    ErrMalVal(MalVal),
}

pub type MalArgs = Vec<MalVal>;
pub type MalRet = Result<MalVal, MalErr>;

pub fn error(s: &str) -> MalRet {
    Err(ErrString(s.to_string()))
}

pub fn func(f: fn(MalArgs) -> MalRet) -> MalVal {
    Func(f, Rc::new(Nil))
}

pub fn atom(mv: &MalVal) -> MalVal {
    Atom(Rc::new(RwLock::new(mv.clone())))
}

impl MalVal {
    pub fn nil() -> Self {
        MalVal::Nil
    }

    pub fn boolean(b: bool) -> Self {
        MalVal::Bool(b)
    }

    pub fn int(i: i64) -> Self {
        MalVal::Int(i)
    }

    pub fn str(s: &str) -> Self {
        MalVal::Str(s.to_string())
    }

    pub fn sym(s: &str) -> Self {
        MalVal::Sym(s.to_string())
    }

    pub fn list(l: &[MalVal]) -> Self {
        MalVal::List(Rc::new(l.to_vec()), Rc::new(MalVal::nil()))
    }

    pub fn vector(l: &[MalVal]) -> Self {
        MalVal::Vector(Rc::new(l.to_vec()), Rc::new(MalVal::nil()))
    }

    pub fn hash(h: &HashMap<String, MalVal>) -> MalVal {
        MalVal::Hash(Rc::new(h.clone()), Rc::new(MalVal::nil()))
    }

    pub fn func(f: fn(MalArgs) -> MalRet) -> MalVal {
        MalVal::Func(f, Rc::new(MalVal::nil()))
    }

    pub fn keyword(&self) -> MalRet {
        match self {
            Str(s) if s.starts_with('\u{29e}') => Ok(Str(s.to_string())),
            Str(s) => Ok(Str(format!("\u{29e}{}", s))),
            _ => error("invalid type for keyword"),
        }
    }

    pub fn keyword_q(&self) -> bool {
        matches!(self, Str(s) if s.starts_with('\u{29e}'))
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
                let fn_env = Env::bind(Some(env.clone()), (**params).clone(), args)?;
                eval(a.clone(), fn_env)
            }
            _ => error("attempt to call non-function"),
        }
    }

    pub fn empty_q(&self) -> MalRet {
        match self {
            List(l, _) | Vector(l, _) => Ok(Bool(l.len() == 0)),
            Str(s) => Ok(Bool(s.is_empty())),
            Nil => Ok(Bool(true)),
            _ => error("invalid type for empty?"),
        }
    }

    pub fn count(&self) -> MalRet {
        match self {
            List(ls, _) | Vector(ls, _) => Ok(Int(ls.len() as i64)),
            Nil => Ok(Int(0)),
            _ => error("invalid type for count"),
        }
    }

    pub fn deref(&self) -> MalRet {
        match self {
            Atom(a) => Ok(a.read().unwrap().clone()),
            _ => error("attempt to deref a non-Atom"),
        }
    }

    pub fn reset_bang(&self, new: &MalVal) -> MalRet {
        match self {
            Atom(a) => {
                *a.write().unwrap() = new.clone();
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
                fargs.insert(0, a.read().unwrap().clone());
                *a.write().unwrap() = f.apply(fargs)?;
                Ok(a.read().unwrap().clone())
            }
            _ => error("attempt to swap! a non-Atom"),
        }
    }

    pub fn get_meta(&self) -> MalRet {
        match self {
            List(_, meta) | Vector(_, meta) | Hash(_, meta) => Ok((**meta).clone()),
            Func(_, meta) => Ok((**meta).clone()),
            MalFunc { meta, .. } => Ok((**meta).clone()),
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
                *meta = Rc::new((*new_meta).clone());
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
            List(_, _) => "list".to_string(),
            Vector(_, _) => "vector".to_string(),
            Hash(_, _) => "hash".to_string(),
            Func(_, _) => "func".to_string(),
            MalFunc { .. } => "mal_func".to_string(),
            Atom(_) => "atom".to_string(),
        }
    }
}

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Str(s) => s.to_string(),
                Sym(s) => s.to_string(),
                _ => panic!(),
            }
        )
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

impl Eq for MalVal {}

impl PartialOrd for MalVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Nil, Nil) => None,
            (Bool(ref a), Bool(ref b)) => Some(a.cmp(b)),
            (Int(ref a), Int(ref b)) => Some(a.cmp(b)),
            (Str(ref a), Str(ref b)) => Some(a.cmp(b)),
            (Sym(ref a), Sym(ref b)) => Some(a.cmp(b)),
            _ => None,
        }
    }
}

pub fn _assoc(mut hm: HashMap<String, MalVal>, kvs: MalArgs) -> MalRet {
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

pub fn _dissoc(mut hm: HashMap<String, MalVal>, ks: MalArgs) -> MalRet {
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
    let hm: HashMap<String, MalVal> = HashMap::default();
    _assoc(hm, kvs)
}
