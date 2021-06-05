use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::sync::Arc;

use mal::mal::rep;
use mal::list;
use mal::core::env_core;
use mal::types::format_error;
use mal::env::env_sets;
use mal::types::MalVal::{List, Str, Nil};

fn main() {
    let repl_env = env_core();

    let args = std::env::args();
    env_sets(&repl_env, "", "*ARGV*", list!(args.map(Str).collect()));

    let _ = rep(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f)\"\nnil)\")))))".to_string(), &repl_env,);

    let _ = rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))".to_string(), &repl_env);

    let _ = rep("(def! *host-language* \"flang\")".to_string(), &repl_env);
    let _ = rep("(println (str \"Mal [\" *host-language* \"]\"))".to_string(), &repl_env);

    let mut rl = Editor::<()>::new();
    if rl.load_history(".flang-history").is_err() {
        //println!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                rl.save_history(".flang-history").unwrap();
                match rep(line, &repl_env) {
                    Ok(out) => println!("{}", out),
                    Err(err) => println!("Error: {}", format_error(err)),
                }
            },

            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}
