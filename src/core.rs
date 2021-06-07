use log::error;
use regex::Regex;
use dyn_fmt::AsStrFormatExt;
use ureq::Error;

use std::fs::File;
use std::io::Read;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use itertools::Itertools;

use num_bigint::{BigInt, Sign};

use std::fs;

use crate::types::{
    MalVal, 
    MalErr, 
    MalArgs, 
    MalRet, 
    atom, 
    hash_map, 
    _dissoc, 
    _assoc,
    Mal
};

use crate::types::MalVal::{
    Nil, 
    Bool, 
    Int, 
    Sym, 
    Str, 
    List, 
    Atom, 
    Hash, 
    Func, 
    MalFunc
};

use crate::types::{error, func, func_doc, int_to_bigint};
use crate::types::MalErr::{ErrMalVal, ErrString};
use crate::mal::rep;

use crate::reader::read_str;

use crate::printer::pr_seq;
use crate::env::{env_new, env_set, env_set_from_vector, Env};

use crate::markdown::{markdown, front_matter};


macro_rules! fn_str {
    ($fn:expr) => {{
        |a: Vec<MalVal>| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => error("expecting (str) arg"),
        }
    }};
}

macro_rules! fn_is_type {
  ($($ps:pat),*) => {{
    |a:Vec<MalVal>| { Ok(Bool(match a[0] { $($ps => true,)* _ => false})) }
  }};
  ($p:pat if $e:expr) => {{
    |a:Vec<MalVal>| { Ok(Bool(match a[0] { $p if $e => true, _ => false})) }
  }};
  ($p:pat if $e:expr,$($ps:pat),*) => {{
    |a:Vec<MalVal>| { Ok(Bool(match a[0] { $p if $e => true, $($ps => true,)* _ => false})) }
  }};
}

fn list_args(a: MalArgs) -> Result<Vec<MalVal>, MalErr> {
    if a.len() == 0 {
        return Err(ErrString("function requires arguments".to_string()));
    }

    Ok(match &a[0] {
        List(v, _) => (**v).clone(),
        _ => a.clone(),
    })
}

fn list_ints(a: MalArgs) -> Result<Vec<BigInt>, MalErr> {
    if a.len() == 0 {
        return Err(ErrString("+: expected arguments".to_string()));
    }

    let vs = match &a[0] {
        List(v, _) => (**v).clone(),
        Int(_) => a.clone(),
        _ => return Err(ErrString(format!("+: unexpected first argument. Expected int or List, got {}", a[0].type_info()))),
    };

    vs.iter().map(|x| match x {
            Int(i) => Ok(i.clone()),
            _ => return Err(ErrString(format!("+: expected int, got {}", x.type_info()))),
        }
    ).collect::<Result<Vec<BigInt>, MalErr>>()
}

fn slurp(f: String) -> MalRet {
    let mut s = String::new();
    match File::open(f.to_string()).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => {println!("slurp: {}", e.to_string()); error(&format!("slurp: {}", &e.to_string()))},
    }
}

fn car(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) => {
            if v.len() == 0 {
                return Ok(Nil);
            }
            Ok(v[0].clone())
        },
        Nil => Ok(Nil),
        _ => error("Trying to find the car of something that is not a list or vector"),
    }
}

fn cdr(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) => {
            if v.len() == 0 {
                return Ok(list![]);
            }
            Ok(list!(v[1..].to_vec()))
        },
        Nil => Ok(list!()),
        _ => error("Trying to find the car of something that is not a list or vector"),
    }
}

fn cons(a: MalArgs) -> MalRet {
    match &a[1] {
        List(v, _) => {
            let mut nv = vec![a[0].clone()];
            nv.extend_from_slice(v);
            Ok(list!(nv.to_vec()))
        }
        _ => error("Called cons with a second argument that is not a list or a vector"),
    }
}

fn concat(a: MalArgs) -> MalRet {
    let mut nv = vec![];
    for seq in a.iter() {
        match seq {
            List(v, _) => nv.extend_from_slice(v),
            _ => return error("non-seq passed to concat"),
        }
    }

    Ok(list!(nv.to_vec()))
}

fn flatten(a: MalArgs) -> MalRet {
    let mut nv = vec![];
    match &a[0] {
        List(vs, _) =>  {
            for v in (**vs).clone().into_iter() {
                match v {
                    List(ref vec, _) => {
                        for x in &**vec {
                            nv.push(x.clone());
                        }
                    },
                    Nil => {},
                    _ => nv.push(v)
                };
            }

            Ok(list![nv])
        }
        _ => error("flatten: called with something that is not a collection"),
    }
}

fn vec(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref l, _) => Ok(list![l.to_vec()]),
        _ => error("Calling vec with something that is not a vector or a list"),
    }
}

fn nth(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (List(seq, _), Int(idx)) => {
            let index: usize = match idx.to_u32_digits() {
                (Sign::NoSign, _) => 0,
                (_, vec) => vec[0] as usize,
            };
            if seq.len() <= index  {
                return error("nth: index out of range");
            }
            Ok(seq[index].clone())
        }
        _=> error("invalid args to nth"),
    }
}

fn apply(a: MalArgs) -> MalRet {
    match a[a.len() - 1] {
        List(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(&v);
            f.apply(fargs)
        }
        _ => error("apply called with non-seq"),
    }
}

fn map(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                res.push(a[0].apply(vec![mv.clone()])?)
            }
            Ok(list!(res))
        },
        _ => error("map: second argument is not a sequence"),
    }
}

fn filter(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                match a[0].apply(vec![mv.clone()])? {
                    Bool(true) => res.push(mv.clone()),
                    _ => continue,
                };
            }
            Ok(list!(res))
        },
        _ => error("filter: second argument is not a sequence"),
    }
}

fn reduce(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) => {
            let mut aux = v[0].clone();
            let mut iter = v.iter();
            let _ = iter.next();
            for mv in iter {
                aux = a[0].apply(vec![aux, mv.clone()])? 
            }
            Ok(aux.clone())
        },
        _ => error("reduce: second argument is not a sequence"),
    }
}

fn fold(a: MalArgs) -> MalRet {
    if a.len() != 3 {
        return error(&format!("fold: expected 3 arguments got {}", a.len()));
    }

    let (f, start, iter) = (a[0].clone(), a[1].clone(), a[2].clone());
    match iter {
        List(ref v, _) => Ok(v.iter().fold(start, |x, y| {f.apply(vec![x, y.clone()]).unwrap()})),
        _ => error(&format!("fold: 3rd argument must be a list, got {}", a[2].type_info())),
    }
}

fn symbol(a: MalArgs) -> MalRet {
    match &a[0] {
        Str(s) => Ok(Sym(s.to_string())),
        _ => error("symbol: called with something that is not a String"),
    }
}

fn type_info(a: MalArgs) -> MalRet {
    Ok(Str(a[0].type_info()))
}

fn get(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Nil, _) => Ok(Nil),
        (Hash(ref hm, _), Str(ref s)) => match hm.get(s) {
            Some(mv) => Ok(mv.clone()),
            None => Ok(Nil),
        },
        _ => error("illegal get args"),
    }
}

fn assoc(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => _assoc((**hm).clone(), a[1..].to_vec()),
        _ => error("assoc on non-Hash Map"),
    }
}

fn dissoc(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => _dissoc((**hm).clone(), a[1..].to_vec()),
        _ => error("dissoc on non-Hash Map"),
    }
}

fn contains_q(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Hash(ref hm, _), Str(ref s)) => Ok(Bool(hm.contains_key(s))),
        _ => error("illegal get args"),
    }
}

fn keys(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list!(hm.keys().map(|k| { Str(k.to_string()) }).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list!(hm.values().map(|v| { v.clone() }).collect())),
        _ => error("keys requires Hash Map"),
    }
}


fn time_ms(_args: MalArgs) -> MalRet {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    
    let millis = since_the_epoch.as_millis();

    let mut v: Vec<u32> = vec![];
    v.push(((millis >> 00) & 0xFFFFFFFF) as u32);
    v.push(((millis >> 32) & 0xFFFFFFFF) as u32);
    v.push(((millis >> 64) & 0xFFFFFFFF) as u32);
    v.push(((millis >> 96) & 0xFFFFFFFF) as u32);

    Ok(Int(BigInt::new(Sign::Plus, v)))
    //Ok(Int(millis as i64))
}

fn seq(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) if v.len() == 0 => Ok(Nil),
        List(ref v, _) => Ok(list!(v.to_vec())),
        Str(ref s) if s.len() == 0 => Ok(Nil),
        Str(ref s) if !a[0].keyword_q() => {
            Ok(list!(s.chars().map(|c| { Str(c.to_string()) }).collect()))
        }
        Nil => Ok(Nil),
        _ => error("seq: called with non-seq"),
    }
}

fn conj(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) => {
            let sl = a[1..]
                .iter()
                .rev()
                .map(|a| a.clone())
                .collect::<Vec<MalVal>>();
            Ok(list!([&sl[..], v].concat()))
        }
        _ => error("conj: called with non-seq"),
    }
}

fn split(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Str(sp), Str(st)) => {
            Ok(list![st.split(&sp[..]).map(|a| Str(a.to_string())).collect::<Vec<MalVal>>()])
        }
        _ => error("split: arguments are not strings"),
    }
}

fn int(a: MalArgs) -> MalRet {
    match &a[0] {
        Str(s) => Ok(Int(int_to_bigint(s.parse::<i64>().unwrap()))),
        _ => error("int: not implemented"),
    }
}

fn combinations(a:MalArgs) -> MalRet {
    let mut r = vec![];
    match (&a[0], &a[1]) {
        (Int(n), List(v, _)) => {
            let (_, ns) = n.to_u32_digits();
            let n = ns[ns.len()-1];
            for c in v.iter().combinations(n as usize) {
                let mut aux = vec![];
                for b in c {
                    aux.push(b.clone());
                }
                r.push(list!(aux.to_vec()));
            }
            Ok(list!(r.to_vec()))
        }
        _ => error("combination: Wrong set of parameters")
    }
}

fn sum(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) => {
            let mut aux = BigInt::new(Sign::NoSign, vec![]);
            for i in v.to_vec() {
                match i {
                    Int(i) => aux = aux + i,
                    _ => {},

                }
            }
            Ok(Int(aux))
        }
        _ => error("sum: input is not sequence")
    }
}

fn mul(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) => {
            let mut aux = BigInt::new(Sign::NoSign, vec![]);
            for i in v.to_vec() {
                match i {
                    Int(i) => aux = aux * i,
                    _ => {},

                }
            }
            Ok(Int(aux))
        }
        _ => error("sum: input is not sequence")
    }
}

fn not_empty(a: MalArgs) -> MalRet {
    match a[0].empty_q() {
        Ok(Bool(b)) => Ok(Bool(!b)),
        _ => error("not_empty: expected a bool"),
    }
}


fn render(a: MalArgs) -> MalRet {
    let (args, s) = match (a[0].clone(), a[1].clone()) {
        (Hash(hm, _) , Str(s)) => (hm, s),
        (Hash(hm, _) , Sym(s)) => (hm, s),
        _ => return error("render: incompatible argument types")
    };

    let env = env_new(None);
    env_set_from_vector(&env, ns());

    for (key, value) in (*args).clone() {
        env_set(&env, Sym(key), value)
            .map_err(|e| error!("{:?}", e))
            .unwrap();
    }

    env_set(&env, Sym("args".to_string()), a[0].clone())
        .map_err(|e| error!("{:?}", e))
        .unwrap();

    let re = Regex::new(r"\{\{((?s).*?)\}\}").unwrap();
    return Ok(Str(re.replace_all(&s, 
        |cap: &regex::Captures| {
            let r = match rep(cap[1].to_string(), &env) {
                Ok(ok) => ok,
                Err(_) => format!("{:?}", cap[1].to_string()),
            };
            r
        }).to_string()));

}


fn mal_markdown(a: MalArgs) -> MalRet {
    let s = a[0].to_string();
    Ok(Hash(Arc::new(markdown(s)), Arc::new(Nil)))
}

fn mal_front_matter(a: MalArgs) -> MalRet {
   let path = match &a[0]  {
       Str(s) => s,
       Sym(s) => s,
       _ => return error("front_matter: expected arguments "),
    };
    
    let (_, meta) = front_matter(fs::read_to_string(path).unwrap());
    Ok(Hash(Arc::new(meta), Arc::new(Nil)))
}

fn replace(a: MalArgs) -> MalRet {
    let (s, old, new) = match (&a[0], &a[1], &a[2]) {
        (Str(s), Str(old), Str(new)) => (s, old, new),
        (Str(s), Str(old), Sym(new)) => (s, old, new),
        (Str(s), Sym(old), Str(new)) => (s, old, new),
        (Str(s), Sym(old), Sym(new)) => (s, old, new),
        (Sym(s), Str(old), Str(new)) => (s, old, new),
        (Sym(s), Str(old), Sym(new)) => (s, old, new),
        (Sym(s), Sym(old), Str(new)) => (s, old, new),
        (Sym(s), Sym(old), Sym(new)) => (s, old, new),
        _ => return error("replace: arguments do not match expected types"),
    };

    Ok(Str(s.replace(old, new)))
}

fn format(a: MalArgs) -> MalRet {
    let mut strs: Vec<&str> = Vec::new();
    let fmt = match &a[0] {
        Str(s) => s,
        _ => return error("format: 1st argument is not a string"),
    };

    for i in 1..a.len() {
        match &a[i] {
            Str(s) => strs.push(&s),
            Sym(s) => strs.push(&s),
            Nil => strs.push("Nil"),
            _ => return error(&format!("format: argument is not a string {:?} {}", &a[i], i)),
        };
    }
    
    Ok(Str(fmt.format(strs)))
}

fn join(a: MalArgs) -> MalRet {
    let (s,v) = match (&a[0], &a[1]) {
        (Str(s), List(v, _)) | (Sym(s), List(v, _)) => (s,v),
        _ => return error("join: wrong input argument types"),
    };

    let strs = (*v).iter().map(|x| x.to_string()).collect::<Vec<String>>();

    Ok(Str((strs).join(&s)))
}

fn mal_in(a: MalArgs) -> MalRet {
    match (&a[0], &a[1]) {
        (Str(pred) | Sym(pred), Str(string) | Sym(string)) => Ok(Bool(string.contains(pred))),
        (_, List(v, _)) => Ok(Bool(v.contains(&a[0]))),
        (_, Nil) => Ok(Bool(false)),
        (Nil, _) => Ok(Bool(false)),
        _ => return error("in: argument types do not match"),
    }

}

pub fn cat(a: MalArgs) -> MalRet {
    let x = match (&a[0], &a[1]) {
        (Str(s1), Str(s2)) => {
            let mut aux: String = "".to_owned();
            aux.push_str(s1);
            aux.push_str(s2);
            aux 
        },
        _ => return error("cat: wrong argument type"),
    };

    return Ok(Str(x.clone()));
}

pub fn path_join(a: MalArgs) -> MalRet {
    match a.iter().map(|x| {
        match x {
            Str(s) | Sym(s) => Ok(s.to_string()),
            _ => Err(ErrString("join: received something that is not a string".to_string())),
        }
    }).collect::<Result<Vec<String>, MalErr>>() {
        Ok(s) => Ok(Str(s.join("/"))),
        Err(err) => Err(err),
    }
}

pub fn curl_get(a: MalArgs) -> MalRet {
    let s = match &a[0] {
        Str(s) => s,
        _ => return error("curl::get received something that is not a string"),
    };


    match ureq::get(s).call() {
        Ok(response) => Ok(Str(response.into_string().unwrap())),
        Err(Error::Status(code, _)) => error(&format!("curl::get {}", code)),
        Err(_) => error("curl::get: error acessing remote resource."),
    }
}

pub fn dedup(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) => {
            let mut vec: Vec<MalVal> = Vec::new();
            for x in &**v {
                if !vec.contains(&x) {
                    vec.push(x.clone())
                }
            }
            Ok(list![vec.clone()])
        }
        _ => error(&format!("dedup: expected List|Vector got {}", a[0].type_info())),
    }
}

fn mal_eq(a: MalArgs) -> MalRet {
    Ok(Bool(a[0] == a[1]))
}

fn mal_neq(a: MalArgs) -> MalRet {
    Ok(Bool(a[0] != a[1]))
}


fn mal_sum(a: MalArgs) -> MalRet {
    Ok(list_args(a)?.iter().fold(0.to_mal(), |x, y| {x + y.clone()}))
}

fn mal_diff(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
        _ => error(&format!("+: unexpected arguments, expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info())),
    }
}

fn mal_product(a: MalArgs) -> MalRet {
    Ok(Int(list_ints(a)?.iter().fold(int_to_bigint(1), |x, y| {x*y}).clone()))
}

fn mal_div(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => {
            Ok(Int(i1/i2))
        }
        _ => error(&format!("/: expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info()))
    }
}

fn mal_bt(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => Ok(Bool(i1 > i2)),
        _ => error(&format!(">: expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info()))
    }
}

fn mal_lt(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => Ok(Bool(i1 < i2)),
        _ => error(&format!("<: expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info()))
    }
}

fn mal_be(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => Ok(Bool(i1 >= i2)),
        _ => error(&format!(">=: expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info()))
    }
}

fn mal_le(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(i1), Int(i2)) => Ok(Bool(i1 <= i2)),
        _ => error(&format!("<=: expected (int, int) got ({}, {})", a[0].type_info(), a[1].type_info()))
    }
}

fn mal_not(a: MalArgs) -> MalRet {
    if a.len() != 1 {
        return error(&format!("not: expected 1 argument received {}", a.len()));
    }
    (!(a[0].clone()))
        .error_nil("not: received something that is not a Boolean.")
}

fn mal_and(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (args?.iter().fold(Bool(true), |x, y| {x & y.clone()}))
        .error_nil("and: received something that is not a Boolean.")
}

fn mal_or(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (args?.iter().fold(Bool(false), |x, y| {x | y.clone()}))
        .error_nil("or: received something that is not a Boolean.")
}

fn mal_nand(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (!args?.iter().fold(Bool(true), |x, y| {x & y.clone()}))
        .error_nil("nand: received something that is not a Boolean.")
}

fn mal_nor(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (!args?.iter().fold(Bool(false), |x, y| {x | y.clone()}))
        .error_nil("nor: received something that is not a Boolean.")
}

fn mal_xor(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (args?.iter().fold(Bool(false), |x, y| {x ^ y.clone()}))
        .error_nil("xor: received something that is not a Boolean.")
}

fn mal_xnor(a: MalArgs) -> MalRet {
    let args = list_args(a);
    (!args?.iter().fold(Bool(false), |x, y| {x ^ y.clone()}))
        .error_nil("xnor: received something that is not a Boolean.")
}

pub fn ns() -> Vec<(&'static str, &'static str, MalVal)> {
    vec![
        ("", "throw", func(|a| Err(ErrMalVal(a[0].clone())))),
        ("", "=", func_doc(mal_eq, "Equality check.")),
        ("", "!=", func_doc(mal_neq, "Inequality check.")),
        ("", "+", func_doc(mal_sum, "Numeric sum.")),
        ("", "-", func_doc(mal_diff, "Numeric difference.")),
        ("", "*", func_doc(mal_product, "Numeric product.")),
        ("", "/", func_doc(mal_div, "Numeric division.")),
        ("", ">", func_doc(mal_bt, "Greater than check.")),
        ("", ">=", func_doc(mal_be, "Greater or equal check.")),
        ("", "<", func_doc(mal_lt, "Lesser than check.")),
        ("", "<=", func_doc(mal_le, "Lesser or equal check.")),
        ("mal", "in", func_doc(mal_in, "Check that value exists in collection.")),
        ("", "not", func_doc(mal_not, "Logical not.")),
        ("", "!", func_doc(mal_not, "Logical not.")),
        ("", "and", func_doc(mal_and, "Logical and.")),
        ("", "or", func_doc(mal_or, "Logical or.")),
        ("", "nand", func_doc(mal_nand, "Logical nand.")),
        ("", "nor", func_doc(mal_nor, "Logical nor.")),
        ("", "xor", func_doc(mal_xor, "Logical xor.")),
        ("", "xnor", func_doc(mal_xnor, "Logical xnor.")),
        ("", "list", func_doc(|a| {Ok(list!(a))}, "Make list.")),
        ("", "list?", func_doc(fn_is_type!(List(_, _)), "Is list?.")),
        ("", "empty?", func_doc(|a| a[0].empty_q(), "Is empty?.")),
        ("", "!empty?", func(not_empty)),
        ("", "count", func_doc(|a| a[0].count(), "Count elements in collection.")),
        ("", "pr-str", func(|a| Ok(Str(pr_seq(&a, true, "", "", " "))))),
        ("", "str", func(|a| Ok(Str(pr_seq(&a, false, "", "", ""))))),
        ("", 
            "prn",
            func(|a| {
                println!("{}", pr_seq(&a, true, "", "", " "));
                Ok(Nil)
            }),
        ),
        ("",
            "println",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }),
        ),
        ("",
            "print",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }),
        ),
        ("", "read-string", func(fn_str!(|s| { read_str(s) }))),
        ("", "slurp", func(fn_str!(|s| { slurp(s) }))),
        ("", "atom", func(|a| {Ok(atom(&a[0]))})),
        ("", "atom?", func(fn_is_type!(Atom(_)))),
        ("", "deref", func(|a| a[0].deref())),
        ("", "reset!", func(|a| a[0].reset_bang(&a[1]))),
        ("", "swap!", func(|a| a[0].swap_bang(&a[1..].to_vec()))),
        ("", "car", func(car)),
        ("", "cdr", func(cdr)),
        ("", "cons", func(cons)),
        ("", "concat", func(concat)),
        ("", "flatten", func(flatten)),
        ("", "quasiquote", func(concat)),
        ("", "vec", func(vec)),
        ("", "nth", func(nth)),
        ("", "first", func(car)),
        ("", "rest", func(cdr)),
        ("", "apply", func(apply)),
        ("", "map", func(map)),
        ("", "filter", func(filter)),
        ("", "reduce", func(reduce)),
        ("", "fold", func(fold)),
        ("", "nil?", func(fn_is_type!(Nil))),
        ("", "true?", func(fn_is_type!(Bool(true)))),
        ("", "false?", func(fn_is_type!(Bool(false)))),
        ("", "symbol?", func(fn_is_type!(Sym(_)))),
        ("", "symbol", func(symbol)),
        ("", "keyword", func(|a| a[0].keyword())),
        ("", "keyword?",
            func(fn_is_type!(Str(ref s) if s.starts_with("\u{29e}"))),
        ),
        ("", "vector", func(|a| Ok(list!(a)))),
        ("", "vector?", func(fn_is_type!(List(_, _)))),
        ("", "sequential?", func(fn_is_type!(List(_, _)))),
        ("", "hash-map", func(|a| hash_map(a))),
        ("", "map?", func(fn_is_type!(Hash(_, _)))),
        ("", "assoc", func(assoc)),
        ("", "dissoc", func(dissoc)),
        ("", "get", func(get)),
        ("", "contains?", func(contains_q)),
        ("", "keys", func(keys)),
        ("", "vals", func(vals)),
        ("", "type", func(type_info)),
        ("", "time-ms", func(time_ms)),
        ("", "meta", func(|a| a[0].get_meta())),
        ("", "with-meta", func(|a| a[0].clone().with_meta(&a[1]))),
        ("", "fn?", func(fn_is_type!(MalFunc{is_macro,..} if !is_macro,Func(_,_,_)))),
        ("", "string?", func(fn_is_type!(Str(ref s) if !s.starts_with("\u{29e}")))),
        ("", "number?", func(fn_is_type!(Int(_)))),
        ("", "seq", func(seq)),
        ("", "conj", func(conj)),
        ("",
            "macro?",
            func(fn_is_type!(MalFunc{is_macro,..} if is_macro)),
        ),
        ("parse", "int", func(int)),
        ("math", "combinations", func(combinations)),
        ("math", "sum", func(sum)),
        ("math", "mul", func(mul)),
        ("string", "split", func(split)),
        ("string", "format", func(format)),
        ("string", "join", func(join)),
        ("string", "replace", func(replace)),
        ("string", "cat", func(cat)),
        ("brian", "render", func(render)),
        ("path", "join", func(path_join)),
        ("curl", "get", func(curl_get)),
        ("mkdown", "markdown", func(mal_markdown)),
        ("mkdown", "front_matter", func(mal_front_matter)),
        ("list", "dedup", func(dedup)),
    ]
}

pub fn env_core() -> Env {
    let env = env_new(None);
    env_set_from_vector(&env, ns());
    env
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::types::Mal;
    use proptest::prelude::*;


    #[test]
    fn test_flatten_lisp() {
        let env = env_new(None);
        env_set_from_vector(&env, ns());

        assert_eq!(
            rep("(flatten (list 1 2 (list 3 4)))".to_string(), &env).unwrap(), 
            "(1 2 3 4)");
    }

    #[test]
    fn test_flatten() {
        let s = Str("ola".to_string());
        let a = 1.to_mal();
        let b = 2.to_mal();
        let c = 3.to_mal();

        let v: Vec<MalVal> = vec![
            s.clone(), 
            list![vec![a.clone(), b.clone(), c.clone()]]];

        let result: Vec<MalVal> = vec![s, a, b, c];

        let result = list![result];
        assert_eq!(flatten(vec![list![v]]).unwrap(), result);
    }

    #[test]
    fn test_not() {
        let proposition = Bool(false);
        assert_ne!(mal_not(vec![proposition.clone()]).unwrap(), proposition);
        let proposition = Bool(true);
        assert_ne!(mal_not(vec![proposition.clone()]).unwrap(), proposition);
    }

    fn half_size() -> BoxedStrategy<i32> {
        (any::<i32>().prop_map(|v| v/2)).boxed()
    }

    fn sqrt_size() -> BoxedStrategy<i32> {
        (any::<i32>().prop_map(|v| ((v as f32).sqrt() as i32))).boxed()
    }

    proptest! {
        #[test]
        fn test_eq_prop(a in any::<i32>()) {
            prop_assert_eq!(mal_eq(vec![a.to_mal(), a.to_mal()]).unwrap(), Bool(true));
        }
        
        #[test]
        fn test_neq_prop(a in any::<i32>()) {
            prop_assert_eq!(mal_neq(vec![a.to_mal(), a.to_mal()]).unwrap(), Bool(false));
        }

        #[test]
        fn test_sum_prop(a in half_size(), b in half_size()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_sum(vec![x,y]).unwrap(), (a+b).to_mal());
        }

        #[test]
        fn test_diff_prop(a in half_size(), b in half_size()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_diff(vec![x,y]).unwrap(), (a-b).to_mal());
        }

        #[test]
        fn test_product_prop(a in sqrt_size(), b in sqrt_size()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_product(vec![x,y]).unwrap(), (a*b).to_mal());
        }

        #[test]
        fn test_div_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_div(vec![x,y]).unwrap(), (a/b).to_mal());
        }

        #[test]
        fn test_lt_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_lt(vec![x,y]).unwrap(), Bool(a < b));
        }

        #[test]
        fn test_bt_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_bt(vec![x,y]).unwrap(), Bool(a > b));
        }

        #[test]
        fn test_le_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_le(vec![x,y]).unwrap(), Bool(a <= b));
        }

        #[test]
        fn test_be_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_eq!(mal_be(vec![x,y]).unwrap(), Bool(a >= b));
        }

        #[test]
        fn test_bt_le_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_ne!(mal_bt(vec![x.clone(),y.clone()]).unwrap(), mal_le(vec![x,y]).unwrap());
        }

        #[test]
        fn test_be_lt_prop(a in any::<i32>(), b in any::<i32>()) {
            let x = a.to_mal();
            let y = b.to_mal();
            prop_assert_ne!(mal_be(vec![x.clone(),y.clone()]).unwrap(), mal_lt(vec![x,y]).unwrap());
        }
    }
}
