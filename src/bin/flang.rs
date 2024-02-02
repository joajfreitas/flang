use clap::Parser;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::env::Args;
use std::fs;
use std::sync::Arc;

use colored::*;

use flang::core::env_core;
use flang::env::{env_sets, Env};
use flang::list;
use flang::mal::rep;
use flang::types::format_error;
use flang::types::MalVal::{List, Nil, Str};

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Parser)]
#[clap(version = "0.1", author = "João Freitas. <joaj.freitas@gmail.com>")]
struct Opts {
    file: Option<String>,
}

fn prelude(repl_env: &Env, args: Args) {
    env_sets(repl_env, "", "*ARGV*", list!(args.map(Str).collect()));

    let _ = rep(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f)\"\nnil)\")))))"
            .to_string(),
        repl_env,
    );

    let _ = rep(
        "
(defmacro! cond\
	(fn* (& xs)\
		(if (nil? xs) \
			(throw \"unexpected end of input\")\
			(if (= (car (car xs)) 'else)\
				(car (cdr (car xs)))\
				(list \
					'if (car (car xs)) \
						(if (not (nil? (cdr (car xs))))\
							(car (cdr (car xs)))\
							(throw \"odd number of forms to cond\")\
						)\
						(cons 'cond (cdr xs))\
				)\
			)\
		)\
	)\
)"
        .to_string(),
        repl_env,
    );

    let _ = rep("(def! *host-language* \"flang\")".to_string(), repl_env);
}

fn repl() -> Result<(), std::io::Error> {
    let repl_env = env_core();

    let args = std::env::args();
    prelude(&repl_env, args);

    let _ = rep(
        "(println (str \"Mal [\" *host-language* \"]\"))".to_string(),
        &repl_env,
    );

    let mut rl = Editor::<()>::new();
    if rl.load_history(".flang-history").is_err() {
        //println!("No previous history.");
    }

    let mut prompt: String = "user> ".to_string();
    loop {
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                rl.save_history(".flang-history").unwrap();
                match rep(line, &repl_env) {
                    Ok(out) => {
                        prompt = "user> ".to_string();
                        println!("{}", out);
                    }
                    Err(err) => {
                        prompt = "error> ".red().to_string();
                        println!("{}", format_error(err));
                    }
                }
            }

            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn file(path: &str) -> Result<(), std::io::Error> {
    let repl_env = env_core();
    prelude(&repl_env, std::env::args());

    let file = fs::read_to_string(path)?;
    println!("{:?}", rep(file, &repl_env));
    Ok(())
}

fn main() -> Result<(), std::io::Error> {
    let opts: Opts = Opts::parse();

    match opts.file {
        Some(path) => file(&path),
        None => repl(),
    }
}
