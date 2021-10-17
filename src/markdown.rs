//extern crate yaml_rust;

use pulldown_cmark::{Parser, Options, html};
use regex::{Regex, Captures};
//use yaml_rust::{YamlLoader, Yaml};
use fnv::FnvHashMap;

use crate::mal::{rep, read_eval};
use crate::env::{env_new, Env, env_set_from_vector};
use crate::types::MalVal;
use crate::types::MalVal::{Sym, Hash};
use crate::core::ns;


pub fn front_matter(input: String) -> (String, FnvHashMap<String, MalVal>) {
    let re = Regex::new(r#"===((?s).*?)==="#).unwrap();

    let mut meta = FnvHashMap::default();

    let cap = re.captures_iter(&input).next();
    
    if cap.is_some() {
        let env = env_new(None);
        env_set_from_vector(&env, ns());
        let r = match read_eval(cap.unwrap()[1].to_string(), &env) {
            Ok(v) => v,
            Err(err) => {
                println!("Error(read_eval): {:?}", err);
                return ("".to_string(), meta)
            },
        };
        
        meta = match r {
            Hash(hm, _) => (*hm).clone(),
            _ => return ("Failed2".to_string(), meta),
        };
    }
    

    let input = re.replace_all(&input, |_cap: &regex::Captures| {""}).to_string();
    (input, meta.clone())
}


pub fn markdown(input: String) -> FnvHashMap<String, MalVal> {
    
    let (input, mut meta) = front_matter(input);

    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);

    let parser = Parser::new_ext(&input, options);

    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    meta.insert("body".to_string(),  Sym(html_output));
    meta.insert("input".to_string(),  Sym(input.to_string()));

    return meta;
}
