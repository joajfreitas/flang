use std::sync::Arc;
use std::sync::RwLock;

//use colored::*;

use fnv::FnvHashMap;

//use crate::types::MalErr::ErrString;
use crate::types::MalErr::ErrString;
use crate::types::MalVal::{List, Nil, Sym, Vector};
use crate::types::{error, MalArgs, MalErr, MalRet, MalVal};

pub type Scope = RwLock<FnvHashMap<String, MalVal>>;

#[derive(Debug)]
pub struct EnvStruct {
    pub data: RwLock<FnvHashMap<String, MalVal>>,
    pub outer: Option<Env>,
}

pub type Env = Arc<EnvStruct>;

pub fn env_new(outer: Option<Env>) -> Env {
    Arc::new(EnvStruct {
        data: RwLock::new(FnvHashMap::default()),
        outer,
    })
}

pub fn env_bind(outer: Option<Env>, mbinds: MalVal, exprs: MalArgs) -> Result<Env, MalErr> {
    let env = env_new(outer);
    match mbinds.clone() {
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
    let s = match key {
        Sym(s) => s,
        _ => {
            return Err(ErrString(
                "Env set called with something that is not a String".to_string(),
            ))
        }
    };

    let mut data = env.data.write().unwrap();
    data.insert(s, value.clone());
    Ok(value)
}

pub fn env_sets(env: &Env, key: &str, value: MalVal) {
    let mut data = env.data.write().unwrap();

    data.insert(key.to_string(), value.clone());
}

pub fn env_set_from_vector(env: &Env, vs: Vec<(&'static str, MalVal)>) {
    for (key, val) in vs {
        env_sets(env, key, val);
    }
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    match (
        env.data.read().unwrap().contains_key(key),
        env.outer.clone(),
    ) {
        (true, _) => Some(env.clone()),
        (false, Some(o)) => env_find(&o, key),
        _ => None,
    }
}

pub fn env_get(env: &Env, key: &MalVal) -> MalRet {
    let key = match key {
        Sym(ref s) => s,
        _ => return error("env_get called with something that is not a String"),
    };

    match env_find(env, key) {
        Some(e) => Ok(e
            .data
            .read()
            .unwrap()
            .get(key)
            .ok_or(ErrString(format!("'{}' not found", key)))?
            .clone()),
        _ => error(&format!("Couldn't find {}", key)),
    }
}
