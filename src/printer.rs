use crate::types::MalVal;
use crate::types::MalVal::{Atom, Bool, Func, Hash, Int, List, MalFunc, Nil, Str, Sym, Vector, Datetime};

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
            },
            Int(i) => i.to_string(),
            Datetime(d) => d.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
            Str(s) => {
                if let Some(stripped) = s.strip_prefix('\u{29e}') {
                    format!(":{}", stripped)
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
                let mut l: Vec<(String, MalVal)> =
                    hm.iter().map(|(a, b)| (a.clone(), b.clone())).collect();
                l.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Greater));
                let l: Vec<MalVal> = l
                    .iter()
                    .flat_map(|(k, v)| vec![Str(k.to_string()), v.clone()])
                    .collect();
                pr_seq(&l, print_readably, "{", "}", " ")
            }
            Func(f, _) => format!("#{:?}", f),
            MalFunc { .. } => "mal function".to_string(),
            Atom(a) => format!("(atom {})", a.borrow().pr_str(print_readably)),
        }
    }
}

pub fn pr_seq(seq: &[MalVal], print_readably: bool, start: &str, end: &str, join: &str) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.pr_str(print_readably)).collect();
    format!("{}{}{}", start, strs.join(join), end)
}
