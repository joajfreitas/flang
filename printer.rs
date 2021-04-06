use crate::types::MalVal;
use crate::types::MalVal::{Atom, Bool, Func, Int, List, MalFunc, Nil, Str, Sym, Vector, Hash};


fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

impl MalVal {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            Nil => "nil".to_string(),
            Bool(b) => match b {
                false => "false".to_string(),
                true => "true".to_string(),
            }
            Int(i) => i.to_string(), 
            Str(s) => {
                if s.starts_with("\u{29e}") {
                    format!(":{}", &s[2..])
                } else if print_readably {
                    format!("\"{}\"", escape_str(s))
                } else {
                    s.clone()
                }
            }
            Sym(s) => s.clone(),
            List(l, _) => pr_seq(l, print_readably, "(", ")", " "),
            Vector(v, _) => pr_seq(v, print_readably, "[", "]", " "),
            Hash(hm, _) => {
                let l: Vec<MalVal> = hm
                    .iter()
                    .flat_map(|(k, v)| vec![Str(k.to_string()), v.clone()])
                    .collect();
                pr_seq(&l, print_readably, "{", "}", " ")
            }
            Func(f, _) => format!("#{:?}", f),
            MalFunc {..} => "mal function".to_string(),
            Atom(a) => format!("(atom {})", a.borrow().pr_str(print_readably))
        }
    }

}

/*
pub fn pr_seq(seq: &Vec<MalVal>, start: &str, end: &str) -> String {
    let atoms: Vec<String> = seq.iter().map(|x| x.pr_str()).collect();
    format!("{}{}{}", start, atoms.join(" "), end)
}
*/


pub fn pr_seq(
    seq: &Vec<MalVal>,
    print_readably: bool,
    start: &str,
    end: &str,
    join: &str,
) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.pr_str(print_readably)).collect();
    format!("{}{}{}", start, strs.join(join), end)
}
