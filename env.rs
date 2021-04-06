use std::cell::RefCell;
use std::rc::Rc;

use colored::*;

use fnv::FnvHashMap;

//use crate::types::MalErr::ErrString;
use crate::types::MalVal::{List, Nil, Sym, Vector};
use crate::types::{error, MalVal, MalRet, MalArgs, MalErr};
use crate::types::MalErr::{ErrString};

#[derive(Debug)]
pub struct EnvStruct {
    data: RefCell<FnvHashMap<String, MalVal>>,
    pub outer: Option<Env>,
}

pub type Env = Rc<EnvStruct>;

pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(FnvHashMap::default()),
        outer: outer,
    })
}

pub fn env_bind(outer: Option<Env>, mbinds: MalVal, exprs: MalArgs) -> Result<Env, MalErr>
{
    let env = env_new(outer);
    match mbinds {
        List(binds, _) | Vector(binds, _) => {
            for (i, b) in binds.iter().enumerate() {
                match b {
                    Sym(s) if s == "&" => {
                        env_set(&env, binds[i + 1].clone(), list!(exprs[i..].to_vec()))?;
                        break;
                    }
                    _ => {
                        env_set(&env, b.clone(), exprs[i].clone())?;
                    }
                }
            }
            Ok(env)
        }
        _ => Err(ErrString("env bind binds not List/Vector".to_string())),
    }
}

pub fn env_set(env: &Env, key: MalVal, value: MalVal) -> MalRet {
    match key {
        Sym(s) => {
            env.data.borrow_mut().insert(s.to_string(), value.clone());
            Ok(value)
        }
        _ => Err(ErrString("Env set called with something that is not a String".to_string()))
    }
}

pub fn env_sets(env: &Env, key: &str, value: MalVal) {
    env.data.borrow_mut().insert(key.to_string(), value.clone());
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    match (env.data.borrow().contains_key(key), env.outer.clone()) {
        (true, _) => Some(env.clone()),
        (false, Some(o)) => env_find(&o, key),
        _ => None,
    }
}

pub fn env_get(env: &Env, key: &MalVal) -> MalRet {
    match key {
        Sym(ref s) => match env_find(env, s) {
            Some(e) => Ok(e
                          .data
                          .borrow()
                          .get(s)
                          .ok_or(ErrString(format!("'{}' not found", s)))?
                          .clone()),
            _ => error(&format!("Couldn't find {}", s.bold())),
        }
        _ => error("env_get called with something that is not a String"),
    }
}
