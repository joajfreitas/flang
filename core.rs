use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use itertools::Itertools;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use num_bigint::{BigInt, Sign};

use crate::types::{MalVal, MalArgs, MalRet, atom, hash_map, _dissoc, _assoc};
use crate::types::MalVal::{Nil, Bool, Int, Sym, Str, Vector, List, Atom, Hash, Func, MalFunc};
use crate::types::{error, func, int_to_bigint};
use crate::types::MalErr::ErrMalVal;

use crate::reader::read_str;

use crate::printer::pr_seq;

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: Vec<MalVal>| match (a[0].clone(), a[1].clone()) {
            (Int(a0), Int(a1)) => Ok($ret($fn(a0, a1))),
            _ => error("expecting (int,int) args"),
        }
    }};
}

macro_rules! fn_t_bool_bool {
    ($ret:ident, $fn:expr) => {{
        |a: Vec<MalVal>| match (a[0].clone(), a[1].clone()) {
            (Bool(a0), Bool(a1)) => Ok($ret($fn(a0, a1))),
            _ => error("expecting (bool,bool) args"),
        }
    }};
}


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


fn slurp(f: String) -> MalRet {
    let mut s = String::new();
    match File::open(f.to_string()).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => {println!("{}", f.to_string()); error(&e.to_string())},
    }
}

fn car(a: MalArgs) -> MalRet {
    match &a[0] {
        Vector(v, _) | List(v, _) => {
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
        Vector(v, _) | List(v, _) => {
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
        List(v, _) | Vector(v, _) => {
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
            Vector(v, _) => nv.extend_from_slice(v),
            _ => return error("non-seq passed to concat"),
        }
    }

    Ok(list!(nv.to_vec()))
}

fn vec(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref l, _) => Ok(vector![l.to_vec()]),
        Vector(_, _) => Ok(a[0].clone()),
        _ => error("Calling vec with something that is not a vector or a list"),
    }
}

fn nth(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (List(seq, _), Int(idx)) | (Vector(seq, _), Int(idx)) => {
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
        List(ref v, _) | Vector(ref v, _) => {
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
        List(ref v, _) | Vector(ref v,_) => {
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
        List(ref v, _) | Vector(ref v,_) => {
            let mut res = vec![];
            for mv in v.iter() {
                match a[0].apply(vec![mv.clone()])? {
                    Bool(true) => res.push(mv.clone()),
                    _ => continue,
                };
            }
            Ok(list!(res))
        },
        _ => error("map: second argument is not a sequence"),
    }
}

fn reduce(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) | Vector(ref v,_) => {
            let mut aux = v[0].clone();
            let mut iter = v.iter();
            let _ = iter.next();
            for mv in iter {
                aux = a[0].apply(vec![aux, mv.clone()])? 
            }
            Ok(aux.clone())
        },
        _ => error("map: second argument is not a sequence"),
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


fn readline(a: MalArgs) -> MalRet {
    let rl: Mutex<Editor<()>> = Mutex::new(Editor::<()>::new());
    //let mut rl = Editor::<()>::new();

    match a[0] {
        Str(ref p) => {
            //match rl.readline(p) {
            match rl.lock().unwrap().readline(p) {
                Ok(mut line) => {
                    // Remove any trailing \n or \r\n
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    Ok(Str(line))
                }
                Err(ReadlineError::Eof) => Ok(Nil),
                Err(e) => error(&format!("{:?}", e)),
            }
        }
        _ => error("readline: prompt is not Str"),
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
        List(ref v, _) | Vector(ref v, _) if v.len() == 0 => Ok(Nil),
        List(ref v, _) | Vector(ref v, _) => Ok(list!(v.to_vec())),
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
        Vector(ref v, _) => Ok(vector!([v, &a[1..]].concat())),
        _ => error("conj: called with non-seq"),
    }
}

fn split(a: MalArgs) -> MalRet {
    let v : Vec<MalVal> = vec![];
    match (a[0].clone(), a[1].clone()) {
        (Str(sp), Str(st)) => {
            Ok(vector![st.split(&sp[..]).map(|a| Str(a.to_string())).collect::<Vec<MalVal>>()])
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
        (Int(n), List(v, _)) | (Int(n), Vector(v, _)) => {
            let (_, ns) = n.to_u32_digits();
            let n = ns[ns.len()-1];
            for c in v.iter().combinations(n as usize) {
                let mut aux = vec![];
                for b in c {
                    aux.push(b.clone());
                }
                r.push(vector!(aux.to_vec()));
            }
            Ok(vector!(r.to_vec()))
        }
        _ => error("combination: Wrong set of parameters")
    }
}

fn sum(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) | Vector(v, _) => {
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
        List(v, _) | Vector(v, _) => {
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

fn not_implemented(a:MalArgs) -> MalRet {
    Err(ErrMalVal(Str("not implemented".to_string())))
}


pub fn ns() -> Vec<(&'static str, MalVal)> {
    vec![
        ("throw", func(|a| Err(ErrMalVal(a[0].clone())))),
        ("=", func(|a| Ok(Bool(a[0] == a[1])))),
        ("!=", func(|a| Ok(Bool(a[0] != a[1])))),
        ("+", func(fn_t_int_int!(Int, |i, j| {i+j}))),
        ("-", func(fn_t_int_int!(Int, |i, j| {i-j}))),
        ("*", func(fn_t_int_int!(Int, |i, j| {i * j}))),
        ("/", func(fn_t_int_int!(Int, |i, j| {i/j}))),
        (">", func(fn_t_int_int!(Bool, |i, j| {i>j}))),
        (">=", func(fn_t_int_int!(Bool, |i, j| {i>=j}))),
        ("<", func(fn_t_int_int!(Bool, |i, j| {i<j}))),
        ("<=", func(fn_t_int_int!(Bool, |i, j| {i<=j}))),
        ("and", func(fn_t_bool_bool!(Bool, |i, j| {i && j}))),
        ("or", func(fn_t_bool_bool!(Bool, |i, j| {i || j}))),
        ("nand", func(fn_t_bool_bool!(Bool, |i, j| {!(i && j)}))),
        ("nor", func(fn_t_bool_bool!(Bool, |i, j| {!(i || j)}))),
        ("xor", func(fn_t_bool_bool!(Bool, |i, j| {i ^ j}))),
        ("xnor", func(fn_t_bool_bool!(Bool, |i, j| {!((i ^ j) as bool)}))),
        ("list", func(|a| {Ok(list!(a))})),
        ("list?", func(fn_is_type!(List(_, _)))),
        ("empty?", func(|a| a[0].empty_q())),
        ("!empty?", func(not_empty)),
        ("count", func(|a| a[0].count())),
        ("pr-str", func(|a| Ok(Str(pr_seq(&a, true, "", "", " "))))),
        ("str", func(|a| Ok(Str(pr_seq(&a, false, "", "", ""))))),
        (
            "prn",
            func(|a| {
                println!("{}", pr_seq(&a, true, "", "", " "));
                Ok(Nil)
            }),
        ),
        (
            "println",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }),
        ),
        (
            "print",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }),
        ),
        ("read-string", func(fn_str!(|s| { read_str(s) }))),
        ("slurp", func(fn_str!(|s| { slurp(s) }))),
        ("atom", func(|a| {Ok(atom(&a[0]))})),
        ("atom?", func(fn_is_type!(Atom(_)))),
        ("deref", func(|a| a[0].deref())),
        ("reset!", func(|a| a[0].reset_bang(&a[1]))),
        ("swap!", func(|a| a[0].swap_bang(&a[1..].to_vec()))),
        ("car", func(car)),
        ("cdr", func(cdr)),
        ("cons", func(cons)),
        ("concat", func(concat)),
        ("quasiquote", func(concat)),
        ("vec", func(vec)),
        ("nth", func(nth)),
        ("first", func(car)),
        ("rest", func(cdr)),
        ("apply", func(apply)),
        ("map", func(map)),
        ("filter", func(filter)),
        ("reduce", func(reduce)),
        ("nil?", func(fn_is_type!(Nil))),
        ("true?", func(fn_is_type!(Bool(true)))),
        ("false?", func(fn_is_type!(Bool(false)))),
        ("symbol?", func(fn_is_type!(Sym(_)))),
        ("symbol", func(symbol)),
        ("keyword", func(|a| a[0].keyword())),
        (
            "keyword?",
            func(fn_is_type!(Str(ref s) if s.starts_with("\u{29e}"))),
        ),
        ("vector", func(|a| Ok(vector!(a)))),
        ("vector?", func(fn_is_type!(Vector(_, _)))),
        ("sequential?", func(fn_is_type!(List(_, _), Vector(_, _)))),
        ("hash-map", func(|a| hash_map(a))),
        ("map?", func(fn_is_type!(Hash(_, _)))),
        ("assoc", func(assoc)),
        ("dissoc", func(dissoc)),
        ("get", func(get)),
        ("contains?", func(contains_q)),
        ("keys", func(keys)),
        ("vals", func(vals)),
        ("type", func(type_info)),
        ("time-ms", func(time_ms)),
        ("meta", func(|a| a[0].get_meta())),
        ("with-meta", func(|a| a[0].clone().with_meta(&a[1]))),
        ("fn?", func(fn_is_type!(MalFunc{is_macro,..} if !is_macro,Func(_,_)))),
        ("string?", func(fn_is_type!(Str(ref s) if !s.starts_with("\u{29e}")))),
        ("number?", func(fn_is_type!(Int(_)))),
        ("seq", func(seq)),
        ("conj", func(conj)),
        ("readline", func(readline)),
        (
            "macro?",
            func(fn_is_type!(MalFunc{is_macro,..} if is_macro)),
        ),
        ("split", func(split)),
        ("int", func(int)),
        ("combinations", func(combinations)),
        ("sum", func(sum)),
        ("mul", func(mul)),
    ]
}
