use std::sync::Arc;
use std::sync::RwLock;

use fnv::FnvHashMap;

use crate::types::MalVal::{List, Nil, Sym, Vector};
use crate::types::{error, MalVal, MalRet, MalArgs, MalErr};
use crate::types::MalErr::{ErrString};

pub type Scope = RwLock<FnvHashMap<String, MalVal>>;

#[derive(Debug)]
pub struct EnvStruct {
    pub data: RwLock<FnvHashMap<String, MalVal>>,
    pub scopes: RwLock<FnvHashMap<String, Scope>>,
    pub outer: Option<Env>,
}

pub type Env = Arc<EnvStruct>;

pub fn env_new(outer: Option<Env>) -> Env {
    Arc::new(EnvStruct {
        data: RwLock::new(FnvHashMap::default()),
        scopes: RwLock::new(FnvHashMap::default()),
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
    let s = match key {
        Sym(s) => s,
        _ => return Err(ErrString("Env set called with something that is not a String".to_string()))
    };

    let split: Vec<&str> = s.split("::").collect();
    
    let (scope, key) = match split.len() {
        1 => ("", split[0]),
        2 => (split[0], split[1]),
        _ => return error("env_get: couldn't decompose key into (scope, key)"),
    };

    if !env.scopes.read().unwrap().contains_key(scope) {
        env.scopes.write().unwrap().insert(scope.to_string(), RwLock::new(FnvHashMap::default()));
    }

    env.scopes.read().unwrap().get(scope).unwrap().write().unwrap().insert(key.to_string(), value.clone());
    Ok(value)
}

pub fn env_sets(env: &Env, scope: &str, key: &str, value: MalVal) {
    let mut scopes = env.scopes.write().unwrap();

    if !scopes.contains_key(scope) {
        scopes.insert(scope.to_string(), RwLock::new(FnvHashMap::default()));
    }

    scopes.get(scope).unwrap().write().unwrap().insert(key.to_string(), value.clone());
}

pub fn env_set_from_vector(env: &Env, vs: Vec<(&'static str, &'static str, MalVal)>) {
    for (scope, key, val) in vs {
        env_sets(env, scope, key, val);
    }
}

pub fn env_find(env: &Env, key: &str, scope: &str) -> Option<Env> {
    let scopes = env.scopes.read().unwrap();
    match (scopes.contains_key(scope), env.outer.clone()) {
        (true, _) => {
            let scope_hm = scopes.get(scope).unwrap();
            match (scope_hm.read().unwrap().contains_key(key), env.outer.clone()) {
                (true, _) => Some(env.clone()),
                (false, Some(o)) => env_find(&o, key, scope),
                _ => None,
            }
        }
        (false, Some(o)) => env_find(&o, key, scope),
        _ => None,
    }
}

pub fn env_list_namespace(env: &Env, namespace: String) -> Option<Vec<String>> {
    if !env.scopes.read().unwrap().contains_key(&namespace) {
        return None; 
    }

    Some(env.scopes
        .read()
        .unwrap()
        .get(&namespace)
        .unwrap()
        .read()
        .unwrap()
        .iter()
        .map(|(x, _)| x.clone())
        .collect::<Vec<String>>()
    )

}

pub fn env_get(env: &Env, key: &MalVal) -> MalRet {
    let key = match key {
        Sym(ref s) => s,
        _ => return error("env_get called with something that is not a String"),
    };

    let split: Vec<&str> = key.split("::").collect();

    let (scope, key) = match split.len() {
        1 => ("", split[0]),
        2 => (split[0], split[1]),
        _ => return error("env_get: couldn't decompose key into (scope, key)"),
    };

    match env_find(env, key, scope) {
        Some(e) => Ok(e
                      .scopes
                      .read()
                      .unwrap()
                      .get(scope)
                      .unwrap()
                      .read()
                      .unwrap()
                      .get(key)
                      .ok_or(ErrString(format!("'{}' not found", key)))?
                      .clone()),
        _ => error(&format!("Couldn't find {}", key)),
    }
}
