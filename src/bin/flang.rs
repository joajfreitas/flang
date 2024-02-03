use clap::Parser;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use colored::*;
use std::env::Args;
use std::fs;

use flang::rep;
use flang::Env;
use flang::{MalErr, MalVal};

/// Flang is a LISP interpreter
#[derive(Parser)]
#[clap(version = "0.1", author = "João Freitas. <joaj.freitas@gmail.com>")]
struct Opts {
    file: Option<String>,
}

pub fn format_error(e: MalErr) -> String {
    match e {
        MalErr::ErrString(s) => format!("{}: {}", "Error".red(), s.clone()),
        MalErr::ErrMalVal(v) => v.pr_str(true),
    }
}

fn prelude(repl_env: &Env, args: Args) {
    let args: Vec<MalVal> = args.map(|s| MalVal::str(&s)).collect();
    repl_env.sets("*ARGV*", MalVal::list(&args));

    let _ = rep(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f)\"\nnil)\")))))"
            ,
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
)",
        repl_env,
    );

    let _ = rep("(def! *host-language* \"flang\")", repl_env);
}

fn repl() -> Result<(), std::io::Error> {
    let repl_env = flang::prelude();

    let args = std::env::args();
    prelude(&repl_env, args);

    let _ = rep(
        "(println (str \"Mal [\" *host-language* \"]\"))",
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
                match rep(&line, &repl_env) {
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
    let repl_env = flang::prelude();
    prelude(&repl_env, std::env::args());

    let file = fs::read_to_string(path)?;
    println!("{:?}", rep(&file, &repl_env));
    Ok(())
}

fn main() -> Result<(), std::io::Error> {
    let opts: Opts = Opts::parse();

    match opts.file {
        Some(path) => file(&path),
        None => repl(),
    }
}
