use std::fmt;
use std::sync::{Arc, RwLock};

use std::collections::HashMap;

use crate::types::MalErr::ErrString;
use crate::types::MalVal::{List, Sym, Vector};
use crate::types::{error, MalArgs, MalErr, MalRet, MalVal};

#[derive(Debug)]
pub struct EnvStruct {
    pub data: RwLock<HashMap<String, MalVal>>,
    pub outer: Option<Env>,
}

/// Env holds the execution environment of the interpreter.
#[derive(Clone)]
pub struct Env(Arc<EnvStruct>);

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.data)
    }
}

impl Env {
    pub fn new(outer: Option<Env>) -> Self {
        Env(Arc::new(EnvStruct {
            data: RwLock::new(HashMap::default()),
            outer,
        }))
    }

    pub fn bind(outer: Option<Env>, mbinds: MalVal, exprs: MalArgs) -> Result<Self, MalErr> {
        let env = Env::new(outer);
        match mbinds.clone() {
            List(binds, _) | Vector(binds, _) => {
                if binds.len() != exprs.len() {
                    return Err(MalErr::ErrString(
                        "binds length doesn't match exprs length".to_string(),
                    ));
                }
                for (i, b) in binds.iter().enumerate() {
                    match b {
                        Sym(s) if s == "&" => {
                            env.set(binds[i + 1].clone(), MalVal::list(&exprs[i..]))?;
                            break;
                        }
                        _ => {
                            env.set(b.clone(), exprs[i].clone())?;
                        }
                    }
                }
                Ok(env)
            }
            _ => Err(ErrString("env bind binds not List/Vector".to_string())),
        }
    }

    pub fn outer(&self) -> &Option<Env> {
        &self.0.outer
    }

    pub fn data(&self) -> &RwLock<HashMap<String, MalVal>> {
        &self.0.data
    }

    pub fn set(&self, key: MalVal, value: MalVal) -> MalRet {
        let s = match key {
            Sym(s) => s,
            _ => {
                return Err(ErrString(
                    "Env set called with something that is not a String".to_string(),
                ))
            }
        };

        self.data().write().unwrap().insert(s, value.clone());
        Ok(value)
    }

    pub fn sets(&self, key: &str, value: MalVal) {
        self.data()
            .write()
            .unwrap()
            .insert(key.to_string(), value.clone());
    }

    pub fn set_from_vector(&self, vs: Vec<(&'static str, MalVal)>) {
        for (key, val) in vs {
            self.sets(key, val);
        }
    }

    pub fn find(&self, key: &str) -> Option<Env> {
        match (
            self.data().read().unwrap().contains_key(key),
            self.outer().clone(),
        ) {
            (true, _) => Some(self.clone()),
            (false, Some(o)) => o.find(key),
            _ => None,
        }
    }

    pub fn get(&self, key: &MalVal) -> MalRet {
        let key = match key {
            Sym(ref s) => s,
            _ => return error("env_get called with something that is not a String"),
        };

        match self.find(key) {
            Some(e) => Ok(e
                .data()
                .read()
                .unwrap()
                .get(key)
                .ok_or(ErrString(format!("'{}' not found", key)))?
                .clone()),
            _ => error(&format!("Couldn't find {}", key)),
        }
    }
}
