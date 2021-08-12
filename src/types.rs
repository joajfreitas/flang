use std::sync::Arc;
use std::sync::RwLock;
use std::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Not};
use itertools::Itertools;

use fnv::FnvHashMap;
use std::net::UdpSocket;
use std::net::TcpStream;
use std::net::TcpListener;
use std::thread::JoinHandle;

use crate::types::MalVal::{Atom, Bool, Func, Int, Float, List, MalFunc, Nil, Str, Sym, Hash, Generator, Extra};
use crate::types::MalErr::{ErrString};
use crate::env::{Env, env_bind};

#[derive(Debug, Clone)]
pub enum Extras {
    UdpSocket(Arc<UdpSocket>),
    Thread(Arc<JoinHandle<()>>),
    TcpStream(Arc<TcpStream>),
    TcpListener(Arc<TcpListener>),
}

#[derive(Debug, Clone)]
pub enum MalVal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Sym(String),
    List(Arc<Vec<MalVal>>, Arc<MalVal>),
    Hash(Arc<FnvHashMap<String, MalVal>>, Arc<MalVal>),
    Func(fn(MalArgs) -> MalRet, Arc<MalVal>, Arc<MalVal>),
    MalFunc {
        eval: fn(ast: MalVal, env: Env) -> MalRet,
        ast: Arc<MalVal>,
        env: Env,
        params: Arc<MalVal>,
        is_macro: bool,
        meta: Arc<MalVal>,
        doc: Arc<MalVal>,
    },
    Atom(Arc<RwLock<MalVal>>),
    Generator(Arc<MalVal>, Arc<MalVal>),
    Extra(Extras)
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

pub fn format_error(e: MalErr) -> String {
    match e {
        MalErr::ErrString(s) => s.clone(),
        MalErr::ErrMalVal(v) => v.pr_str(true),
    }
}

pub fn func(f: fn(MalArgs) -> MalRet) -> MalVal {
    Func(f, Arc::new(Nil), Arc::new(Str("Placeholder".to_string())))
}

pub fn func_doc(f: fn(MalArgs) -> MalRet, doc: &str) -> MalVal {
    Func(f, Arc::new(Nil), Arc::new(Str(doc.to_string())))
}

pub fn atom(mv: &MalVal) -> MalVal{
    Atom(Arc::new(RwLock::new(mv.clone())))
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
            Func(f, _, _) => f(args),
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
            List(l, _) => Ok(Bool(l.len() == 0)),
            Str(s) => Ok(Bool(s.len() == 0)),
            Nil => Ok(Bool(true)),
            _ => error("invalid type for empty?"),
        }
    }

    pub fn count(&self) -> MalRet {
        match self {
            List(ls,  _) => Ok(Int(ls.len() as i64)),
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
            List(_, meta) | Hash(_, meta) => Ok((&**meta).clone()),
            Func(_, meta, _) => Ok((&**meta).clone()),
            MalFunc { meta, .. } => Ok((&**meta).clone()),
            _ => error("meta not supported by type"),
        }
    }

    pub fn with_meta(&mut self, new_meta: &MalVal) -> MalRet {
        match self {
            List(_, ref mut meta)
            | Hash(_, ref mut meta)
            | Func(_, ref mut meta, _)
            | MalFunc { ref mut meta, .. } => {
                *meta = Arc::new((&*new_meta).clone());
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
            Float(_) => "float".to_string(),
            Str(_) => "str".to_string(),
            Sym(_) => "sym".to_string(),
            List(_,_) => "list".to_string(),
            Hash(_,_) => "hash".to_string(),
            Func(_,_,_) => "func".to_string(),
            MalFunc {..} => "mal_func".to_string(),
            Atom(_) => "atom".to_string(),
            Generator(_, _) => "generator".to_string(),
            Extra(_) => "extra".to_string(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Str(s) => s.to_string(),
            Sym(s) => s.to_string(),
             _=> {
                println!("type: {}", self.type_info());
                //panic!();
                "failed".to_string()
            }
        }
    }


    pub fn error_nil(&self, s: &str) -> MalRet {
        match self {
            Nil => Err(ErrString(s.to_string())),
            _ => Ok(self.clone()),
        }
    }

    pub fn int(&self, s: &str) -> Result<i64, MalErr> {
        match self {
            Int(i) => Ok(*i),
            _ => Err(ErrString(s.to_string())),
        }
    }

    pub fn extra(&self, s: &str) -> Result<Extras, MalErr>{
        match self {
            Extra(ex) => Ok(ex.clone()),
            _ => Err(ErrString(s.to_string())),
        }
    }

    pub fn str(&self, s: &str) -> Result<String, MalErr> {
        match self {
            Str(st) => Ok(st.to_string()),
            _ => Err(ErrString(s.to_string())),
        }
    }
    
    pub fn is_malfunc(&self) -> bool {
        match self {
            MalFunc {..} => true,
            _ => false,
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
            (List(ref a, _), List(ref b, _)) => a == b,
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
    Ok(Hash(Arc::new(hm), Arc::new(Nil)))
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
    Ok(Hash(Arc::new(hm), Arc::new(Nil)))
}

pub fn hash_map(kvs: MalArgs) -> MalRet {
    let hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
    _assoc(hm, kvs)
}

pub trait Mal {
    fn to_mal(&self) -> MalVal;
}

impl Mal for i32 {
    fn to_mal(&self) -> MalVal {
        Int(*self as i64)
    }
}

impl Mal for i64 {
    fn to_mal(&self) -> MalVal {
        Int(*self)
    }
}

impl Mal for u8 {
    fn to_mal(&self) -> MalVal {
        Int(*self as i64)
    }
}

impl Add for MalVal {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Int(i1), Int(i2)) => Int(i1+i2),
            (Float(i1), Float(i2)) => Float(i1+i2),
            (Int(i1), Float(i2)) => Float((i1 as f64)+i2),
            (Float(i1), Int(i2)) => Float(i1+(i2 as f64)),
            _ => Nil,
        }
    }
}

impl Sub for MalVal {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Int(i1), Int(i2)) => Int(i1-i2),
            (Float(i1), Float(i2)) => Float(i1-i2),
            (Int(i1), Float(i2)) => Float((i1 as f64)-i2),
            (Float(i1), Int(i2)) => Float(i1-(i2 as f64)),
            _ => Nil,
        }
    }
}

impl Mul for MalVal {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Int(i1), Int(i2)) => Int(i1*i2),
            (Float(i1), Float(i2)) => Float(i1*i2),
            (Int(i1), Float(i2)) => Float((i1 as f64)*i2),
            (Float(i1), Int(i2)) => Float(i1*(i2 as f64)),
            _ => Nil,
        }
    }
}

impl Div for MalVal {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Int(i1), Int(i2)) => Int(i1/i2),
            (Float(i1), Float(i2)) => Float(i1/i2),
            (Int(i1), Float(i2)) => Float((i1 as f64)/i2),
            (Float(i1), Int(i2)) => Float(i1/(i2 as f64)),
            _ => Nil,
        }
    }
}

impl BitAnd for MalVal {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bool(b1), Bool(b2)) => Bool(b1 && b2),
            _ => Nil
        }
    }
}

impl BitOr for MalVal {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bool(b1), Bool(b2)) => Bool(b1 || b2),
            _ => Nil
        }
    }
}

impl Not for MalVal {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Bool(true) => Bool(false),
            Bool(false) => Bool(true),
            _ => Nil,
        }
    }
}

impl BitXor for MalVal {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bool(b1), Bool(b2)) => Bool(b1 ^ b2),
            _ => Nil,
        }
    }
}

impl Extras {
    pub fn to_string(&self) -> String {
        "extras".to_string()
    }

    pub fn udp_socket(&self, s: &str) -> Result<Arc<UdpSocket>, MalErr> {
        match self {
            Extras::UdpSocket(s) => Ok(s.clone()),
            _ => return Err(ErrString(s.to_string())),
        }
    }
}
