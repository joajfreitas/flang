//use dyn_fmt::AsStrFormatExt;
use itertools::Itertools;
use rand::{random, thread_rng, Rng};

use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::env::Env;
use crate::printer::pr_seq;
use crate::reader::read_str;
use crate::types::MalErr::{ErrMalVal, ErrString};
use crate::types::MalVal::{Atom, Bool, Func, Hash, Int, List, MalFunc, Nil, Str, Sym, Vector};
use crate::types::{atom, hash_map, MalArgs, MalErr, MalRet, MalVal, _assoc, _dissoc};
use crate::types::{error, func};

macro_rules! fn_t_int_int {
    ($ret:ident, |$x: ident, $y: ident|{$fn:expr}) => {{
        |a: Vec<MalVal>| match (a[0].clone(), a[1].clone()) {
            (Int($x), Int($y)) => Ok($ret($fn)),
            _ => error("expecting (int,int) args"),
        }
    }};
}

macro_rules! fn_t_bool_bool {
    ($ret:ident, |$x:ident, $y:ident| {$fn:expr}) => {{
        |a: Vec<MalVal>| match (a[0].clone(), a[1].clone()) {
            (Bool($x), Bool($y)) => Ok($ret($fn)),
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
    match File::open(&f).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => {
            println!("slurp: {}", e);
            error(&format!("slurp: {}, {}", e, f))
        }
    }
}

// (render "some_template.f" meta)
/*
fn render(a: MalArgs) -> MalRet {
    let meta = match &a[1] {
        Hash(ref hm, _) => hm,
        _ => panic!(),
    };

    let file = match &a[0] {
        Str(s) => s,
        _ => panic!(),
    };

    let mut s = String::new();
    match File::open(file.to_string()).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => {println!("{}", file.to_string()); error(&e.to_string())},
    }
}
*/

fn car(a: MalArgs) -> MalRet {
    match &a[0] {
        Vector(v, _) | List(v, _) => {
            if v.len() == 0 {
                return error("You cannot ask for the car of an empty list.");
            }
            Ok(v[0].clone())
        }
        Nil => Ok(Nil),
        _ => error("The Law of Car: the primitive car is defined only for non-empty lists."),
    }
}

fn cdr(a: MalArgs) -> MalRet {
    match &a[0] {
        Vector(v, _) | List(v, _) => {
            if v.len() == 0 {
                return error("You cannot ask for the cdr of an empty list.");
            }
            Ok(MalVal::list(&v[1..]))
        },
        Nil => Ok(MalVal::list(&[])),
        _ => error("The Law of Cdr:\nThe primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list."),
    }
}

fn cons(a: MalArgs) -> MalRet {
    match &a[1] {
        List(v, _) | Vector(v, _) => {
            let mut nv = vec![a[0].clone()];
            nv.extend_from_slice(v);
            Ok(MalVal::list(&nv.to_vec()))
        }
        _ => error("The Law of Cons:\nThe primitive cons takes two arguments. The second argument to cons must be a list. The result is a list."),
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

    Ok(MalVal::list(&nv.to_vec()))
}

fn vec(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref l, _) => Ok(MalVal::vector(&l.to_vec())),
        Vector(_, _) => Ok(a[0].clone()),
        _ => error("Calling vec with something that is not a vector or a list"),
    }
}

fn nth(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (List(seq, _), Int(idx)) | (Vector(seq, _), Int(idx)) => {
            if seq.len() <= idx as usize {
                return error("nth: index out of range");
            }
            Ok(seq[idx as usize].clone())
        }
        _ => error("invalid args to nth"),
    }
}

fn apply(a: MalArgs) -> MalRet {
    match a[a.len() - 1] {
        List(ref v, _) | Vector(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(v);
            f.apply(fargs)
        }
        _ => error("apply called with non-seq"),
    }
}

fn map(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                res.push(a[0].apply(vec![mv.clone()])?)
            }
            Ok(MalVal::list(&res))
        }
        _ => error("map: second argument is not a sequence"),
    }
}

fn filter(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                match a[0].apply(vec![mv.clone()])? {
                    Bool(true) => res.push(mv.clone()),
                    _ => continue,
                };
            }
            Ok(MalVal::list(&res))
        }
        _ => error("map: second argument is not a sequence"),
    }
}

fn reduce(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut aux = v[0].clone();
            let mut iter = v.iter();
            let _ = iter.next();
            for mv in iter {
                aux = a[0].apply(vec![aux, mv.clone()])?
            }
            Ok(aux.clone())
        }
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
        (_, Nil) => Ok(Nil),
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
        Hash(ref hm, _) => Ok(MalVal::list(&hm.keys().map(|k| { Str(k.to_string()) }).collect::<Vec<MalVal>>())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(MalVal::list(&hm.values().cloned().collect::<Vec<MalVal>>())),
        _ => error("keys requires Hash Map"),
    }
}

fn time_ms(_args: MalArgs) -> MalRet {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    let millis = since_the_epoch.as_millis();

    Ok(Int(millis as i64))
}

fn seq(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) if v.len() == 0 => Ok(Nil),
        List(ref v, _) | Vector(ref v, _) => Ok(MalVal::list(&v.to_vec())),
        Str(ref s) if s.is_empty() => Ok(Nil),
        Str(ref s) if !a[0].keyword_q() => {
            let v: Vec<MalVal> = s.chars().map(|c| { Str(c.to_string()) }).collect();
            Ok(MalVal::list(&v))
        }
        Nil => Ok(Nil),
        _ => error("seq: called with non-seq"),
    }
}

fn conj(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) => {
            let sl = a[1..].iter().rev().cloned().collect::<Vec<MalVal>>();
            Ok(MalVal::list(&[&sl[..], v].concat()))
        }
        Vector(ref v, _) => Ok(MalVal::vector(&[v, &a[1..]].concat())),
        _ => error("conj: called with non-seq"),
    }
}

fn split(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Str(sp), Str(st)) => Ok(MalVal::vector(&st
            .split(&sp[..])
            .map(|a| Str(a.to_string()))
            .collect::<Vec<MalVal>>())),
        _ => error("split: arguments are not strings"),
    }
}

fn int(a: MalArgs) -> MalRet {
    match &a[0] {
        Str(s) => Ok(Int(s.parse::<i64>().unwrap())),
        _ => error("int: not implemented"),
    }
}

fn combinations(a: MalArgs) -> MalRet {
    let mut r = vec![];
    match (&a[0], &a[1]) {
        (Int(n), List(v, _)) | (Int(n), Vector(v, _)) => {
            for c in v.iter().combinations(*n as usize) {
                let mut aux = vec![];
                for b in c {
                    aux.push(b.clone());
                }
                r.push(MalVal::vector(&aux.to_vec()));
            }
            Ok(MalVal::vector(&r.to_vec()))
        }
        _ => error("combination: Wrong set of parameters"),
    }
}

fn sum(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) | Vector(v, _) => {
            let mut aux = 0;
            for i in v.iter() {
                if let Int(i) = i {
                    aux += i;
                }
            }
            Ok(Int(aux))
        }
        _ => error("sum: input is not sequence"),
    }
}

fn mul(a: MalArgs) -> MalRet {
    match &a[0] {
        List(v, _) | Vector(v, _) => {
            let mut aux: i64 = 1;
            for i in v.iter() {
                if let Int(i) = i {
                    aux *= i;
                }
            }
            Ok(Int(aux))
        }
        _ => error("mul: input is not sequence"),
    }
}

fn not_empty(a: MalArgs) -> MalRet {
    match a[0].empty_q() {
        Ok(Bool(b)) => Ok(Bool(!b)),
        _ => error("not_empty: expected a bool"),
    }
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

fn format(args: MalArgs) -> MalRet {
    //let mut strs: Vec<&str> = Vec::new();
    let fmt = match &args[0] {
        Str(s) => s,
        _ => return error("format: 1st argument is not a string"),
    };

    Ok(Str(fmt.clone()))

    //for arg in args.iter().skip(1) {
    //    match arg {
    //        Str(s) => strs.push(s),
    //        Sym(s) => strs.push(s),
    //        Nil => strs.push("Nil"),
    //        _ => return error(&format!("format: argument is not a string {:?}", arg)),
    //    };
    //}

    //Ok(Str(fmt.format(strs)))
}

fn join(a: MalArgs) -> MalRet {
    let (s, v) = match (&a[0], &a[1]) {
        (Str(s), List(v, _)) | (Sym(s), List(v, _)) => (s, v),
        _ => return error("join: wrong input argument types"),
    };

    let strs = (*v).iter().map(|x| x.to_string()).collect::<Vec<String>>();

    Ok(Str((strs).join(s)))
}

fn mal_in(a: MalArgs) -> MalRet {
    match (&a[0], &a[1]) {
        (Nil, _) | (_, Nil) => {
            return Ok(Bool(false));
        }
        (_, List(list, _)) | (_, Vector(list, _)) => {
            return Ok(Bool(list.contains(&a[0])));
        }
        _ => {}
    }

    let (pred, string) = match (&a[0], &a[1]) {
        (Str(pred), Str(string)) => (pred, string),
        (Sym(pred), Str(string)) => (pred, string),
        (Str(pred), Sym(string)) => (pred, string),
        (Sym(pred), Sym(string)) => (pred, string),
        _ => {
            return error(&format!(
                "in: argument types do not match ({:?}, {:?})",
                a[0], a[1]
            ))
        }
    };

    Ok(Bool(string.contains(pred)))
}

pub fn mal_not(a: MalArgs) -> MalRet {
    let x = match &a[0] {
        Bool(b) => !b,
        _ => return error(""),
    };

    Ok(Bool(x))
}

pub fn cat(a: MalArgs) -> MalRet {
    let x = match (&a[0], &a[1]) {
        (Str(s1), Str(s2)) => {
            let mut aux: String = "".to_owned();
            aux.push_str(s1);
            aux.push_str(s2);
            aux
        }
        _ => return error("cat: wrong argument type"),
    };

    Ok(Str(x.clone()))
}

pub fn path_join(a: MalArgs) -> MalRet {
    match a
        .iter()
        .map(|x| match x {
            Str(s) | Sym(s) => Ok(s.to_string()),
            _ => Err(ErrString(
                format!(
                    "path::join: received something that is not a string. {:?}",
                    x
                )
                .to_string(),
            )),
        })
        .collect::<Result<Vec<String>, MalErr>>()
    {
        Ok(s) => Ok(Str(s.join("/"))),
        Err(err) => Err(err),
    }
}

pub fn dedup(a: MalArgs) -> MalRet {
    let mut ys: Vec<MalVal> = Vec::new();

    match &a[0] {
        List(vs, _) | Vector(vs, _) => {
            for v in vs.iter() {
                if !ys.contains(v) {
                    ys.push(v.clone());
                }
            }
            Ok(MalVal::vector(&ys))
        }
        _ => error("list::dedup received something that is not a sequence"),
    }
}

pub fn flatten(a: MalArgs) -> MalRet {
    let mut xs: Vec<MalVal> = Vec::new();

    match &a[0] {
        List(vs, _) | Vector(vs, _) => {
            for v in vs.iter() {
                match v {
                    List(us, _) | Vector(us, _) => {
                        for u in us.iter() {
                            xs.push(u.clone());
                        }
                    }
                    _ => {}
                };
            }
        }
        _ => {}
    };

    Ok(MalVal::vector(&xs))
}

pub fn mal_random(_a: MalArgs) -> MalRet {
    let x: i32 = random();
    Ok(Int(x as i64))
}

pub fn mal_randrange(a: MalArgs) -> MalRet {
    let mut rng = thread_rng();

    match (&a[0], &a[1]) {
        (Int(i1), Int(i2)) => Ok(Int(rng.gen_range(*i1..*i2))),
        _ => error("randrange"),
    }
}

pub fn mal_sys_exit(a: MalArgs) -> MalRet {
    match &a[0] {
        Int(i1) => std::process::exit((*i1).try_into().unwrap()),
        _ => error("exit: expected an integer"),
    }
}

pub fn mal_is_nil(a: MalArgs) -> MalRet {
    match &a[0] {
        Nil => Ok(Bool(true)),
        List(l, _) => {
            if l.len() == 0 {
                Ok(Bool(true))
            } else {
                Ok(Bool(false))
            }
        }
        _ => Ok(Bool(false)),
    }
}

pub fn ns() -> Vec<(&'static str, MalVal)> {
    vec![
        ("throw", func(|a| Err(ErrMalVal(a[0].clone())))),
        ("=", func(|a| Ok(Bool(a[0] == a[1])))),
        ("!=", func(|a| Ok(Bool(a[0] != a[1])))),
        ("+", func(fn_t_int_int!(Int, |i, j| { i + j }))),
        ("-", func(fn_t_int_int!(Int, |i, j| { i - j }))),
        ("*", func(fn_t_int_int!(Int, |i, j| { i * j }))),
        ("/", func(fn_t_int_int!(Int, |i, j| { i / j }))),
        (">", func(fn_t_int_int!(Bool, |i, j| { i > j }))),
        (">=", func(fn_t_int_int!(Bool, |i, j| { i >= j }))),
        ("<", func(fn_t_int_int!(Bool, |i, j| { i < j }))),
        ("<=", func(fn_t_int_int!(Bool, |i, j| { i <= j }))),
        ("in", func(mal_in)),
        ("not", func(mal_not)),
        ("and", func(fn_t_bool_bool!(Bool, |i, j| { i && j }))),
        ("or", func(fn_t_bool_bool!(Bool, |i, j| { i || j }))),
        ("nand", func(fn_t_bool_bool!(Bool, |i, j| { !(i && j) }))),
        ("nor", func(fn_t_bool_bool!(Bool, |i, j| { !(i || j) }))),
        ("xor", func(fn_t_bool_bool!(Bool, |i, j| { i ^ j }))),
        ("xnor", func(fn_t_bool_bool!(Bool, |i, j| { !(i ^ j) }))),
        ("list", func(|a| Ok(MalVal::list(&a)))),
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
        ("read-string", func(fn_str!(read_str))),
        ("slurp", func(fn_str!(slurp))),
        ("read", func(fn_str!(slurp))),
        ("atom", func(|a| Ok(atom(&a[0])))),
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
        ("nil?", func(mal_is_nil)),
        ("true?", func(fn_is_type!(Bool(true)))),
        ("false?", func(fn_is_type!(Bool(false)))),
        ("symbol?", func(fn_is_type!(Sym(_)))),
        ("symbol", func(symbol)),
        ("keyword", func(|a| a[0].keyword())),
        (
            "keyword?",
            func(fn_is_type!(Str(ref s) if s.starts_with('\u{29e}'))),
        ),
        ("vector", func(|a| Ok(MalVal::vector(&a)))),
        ("vector?", func(fn_is_type!(Vector(_, _)))),
        ("sequential?", func(fn_is_type!(List(_, _), Vector(_, _)))),
        ("hash-map", func(hash_map)),
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
        (
            "fn?",
            func(fn_is_type!(MalFunc{is_macro,..} if !is_macro,Func(_,_))),
        ),
        (
            "string?",
            func(fn_is_type!(Str(ref s) if !s.starts_with('\u{29e}'))),
        ),
        ("number?", func(fn_is_type!(Int(_)))),
        ("seq", func(seq)),
        ("conj", func(conj)),
        (
            "macro?",
            func(fn_is_type!(MalFunc{is_macro,..} if is_macro)),
        ),
        ("int", func(int)),
        ("combinations", func(combinations)),
        ("sum", func(sum)),
        ("mul", func(mul)),
        ("split", func(split)),
        ("format", func(format)),
        ("join", func(join)),
        ("replace", func(replace)),
        ("cat", func(cat)),
        ("join", func(path_join)),
        ("dedup", func(dedup)),
        ("flatten", func(flatten)),
        ("random", func(mal_random)),
        ("randrange", func(mal_randrange)),
        ("exit", func(mal_sys_exit)),
    ]
}

pub fn prelude() -> Env {
    let env = Env::new(None);
    env.set_from_vector(ns());
    env
}
