extern crate regex;

use regex::{Captures, Regex};

use crate::types::hash_map;
use crate::types::MalErr::ErrString;
use crate::types::MalVal::{Bool, Int, Nil, Str, Sym};
use crate::types::{error, MalErr, MalRet, MalVal};

#[derive(Debug, Clone)]
struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn next(&mut self) -> Option<String> {
        self.pos += 1;
        Some(self.tokens.get(self.pos - 1).unwrap().to_string())
    }
    fn peek(&self) -> Result<String, MalErr> {
        Ok(self
            .tokens
            .get(self.pos)
            .ok_or(ErrString("Unexpected end of input".to_string()))?
            .to_string())
    }
}

pub fn read_str(str: String) -> MalRet {
    let tokens = tokenize(str);
    if tokens.is_empty() {
        return error("no input");
    }
    let mut reader = Reader { tokens, pos: 0 };

    read_form(&mut reader)
}

fn tokenize(line: String) -> Vec<String> {
    let re =
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"###)
            .unwrap();

    let mut tokens: Vec<String> = Vec::new();
    for cap in re.captures_iter(&line) {
        if cap[1].starts_with(';') {
            continue;
        }
        tokens.push(cap[1].to_string());
    }
    tokens
}

fn read_form(reader: &mut Reader) -> MalRet {
    let token = reader.peek().unwrap();
    match &token[..] {
        "'" => {
            let _ = reader.next();
            Ok(MalVal::list(&[Sym("quote".to_string()), read_form(reader)?]))
        }
        "`" => {
            let _ = reader.next();
            Ok(MalVal::list(&[Sym("quasiquote".to_string()), read_form(reader)?]))
        }
        "~" => {
            let _ = reader.next();
            Ok(MalVal::list(&[Sym("unquote".to_string()), read_form(reader)?]))
        }
        "~@" => {
            let _ = reader.next();
            Ok(MalVal::list(&[Sym("splice-unquote".to_string()), read_form(reader)?]))
        }

        "@" => {
            let _ = reader.next();
            Ok(MalVal::list(&[Sym("deref".to_string()), read_form(reader)?]))
        }
        "^" => {
            let _ = reader.next();
            let meta = read_form(reader)?;
            Ok(MalVal::list(&[
                Sym("with-meta".to_string()),
                read_form(reader)?,
                meta
            ]))
        }
        "(" => read_seq(reader, ")"),
        "[" => read_seq(reader, "]"),
        "{" => read_seq(reader, "}"),
        ")" => {
            println!("unexpected ')'");
            panic!();
        }
        _ => read_atom(reader),
    }
}

fn read_seq(reader: &mut Reader, end: &str) -> MalRet {
    let mut seq: Vec<MalVal> = Vec::new();
    reader.next();
    loop {
        let token = match reader.peek() {
            Ok(t) => t,
            Err(err) => return Err(err),
        };

        if token == end {
            break;
        }
        seq.push(read_form(reader)?)
    }

    let _ = reader.next();
    match end {
        ")" => Ok(MalVal::list(&seq)),
        "]" => Ok(MalVal::vector(&seq)),
        "}" => hash_map(seq),
        _ => error("read_seq unknown end value"),
    }
}

fn unescape_str(s: &str) -> String {
    let re: Regex = Regex::new(r#"\\(.)"#).unwrap();
    re.replace_all(s, |caps: &Captures| {
        (if &caps[1] == "n" { "\n" } else { &caps[1] }).to_string()
    })
    .to_string()
}

fn read_atom(reader: &mut Reader) -> MalRet {
    let int_re: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
    let str_re: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap();

    let token = reader.next().unwrap();
    match &token[..] {
        "nil" => Ok(Nil),
        "false" => Ok(Bool(false)),
        "true" => Ok(Bool(true)),
        _ => {
            if int_re.is_match(&token) {
                Ok(Int(token.parse::<i64>().unwrap()))
            } else if str_re.is_match(&token) {
                Ok(Str(unescape_str(&token[1..token.len() - 1])))
            } else if token.starts_with('"') {
                error("expected '\"', got EOF")
            } else if let Some(stripped) = token.strip_prefix(':') {
                Ok(Str(format!("\u{29e}{}", stripped)))
            } else {
                Ok(Sym(token))
            }
        }
    }
    //Some(Nil)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> Reader {
        Reader {
            tokens: vec!["+".to_string(), "1".to_string(), "2".to_string()],
            pos: 0,
        }
    }

    #[test]
    fn test_next() {
        let mut reader: Reader = setup();
        assert!(reader.next().unwrap() == "+");
        assert_eq!(reader.pos, 1);
    }

    #[test]
    fn test_peek() {
        let reader: Reader = setup();
        assert!(reader.peek().unwrap() == "+");
        assert_eq!(reader.pos, 0);
    }

    #[test]
    fn test_tokenize() {
        let test_line: String = "(+ 1 2)".to_string();
        let tokens: Vec<String> = tokenize(test_line);

        assert_eq!(
            tokens,
            vec![
                "(".to_string(),
                "+".to_string(),
                "1".to_string(),
                "2".to_string(),
                ")".to_string()
            ]
        );
    }
}
