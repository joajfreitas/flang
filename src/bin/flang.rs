use clap::{AppSettings, Clap};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::sync::Arc;
use std::env::Args;
use std::fs;

use colored::*;

use mal::mal::rep;
use mal::list;
use mal::core::env_core;
use mal::types::format_error;
use mal::env::{env_sets, Env};
use mal::types::MalVal::{List, Str, Nil};

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Clap)]
#[clap(version = "0.1", author = "João Freitas. <joaj.freitas@gmail.com>")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    #[clap(version = "0.1", author = "João Freitas <joaj.freitas@gmail.com")]
    File(File),
    Repl(Repl),
}

/// Repl
#[derive(Clap, Debug)]
struct Repl {
}

/// Run file
#[derive(Clap, Debug)]
struct File {
    /// Input file,
    source: String,
}

fn prelude(repl_env: &Env, args: Args) {
    env_sets(&repl_env, "", "*ARGV*", list!(args.map(Str).collect()));

    let _ = rep(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f)\"\nnil)\")))))".to_string(), &repl_env,);

let _ = rep("\
(defmacro! cond \
	(fn* (& xs) \
		(if (nil? xs) \
			(throw \"unexpected end of input\")\
			(if (= (car (car xs)) 'else)\
				(car (cdr (car xs)))\
				(list \
					'if (car (car xs)) \
						(if (not (nil? (cdr (car xs))))\
							(car (cdr (car xs)))\
							(throw \"odd number of forms to cond\")\
						) \
						(cons 'cond (cdr xs))\
				)\
			)\
		)\
	)\
)\
".to_string(), &repl_env);

    let _ = rep("(def! *host-language* \"flang\")".to_string(), &repl_env);

}


fn repl(_args: Repl) -> Result<(), std::io::Error> {
    let repl_env = env_core();

    let args = std::env::args();
    prelude(&repl_env, args);

    let _ = rep("(println (str \"Mal [\" *host-language* \"]\"))".to_string(), &repl_env);

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
                    },
                    Err(err) => {
                        prompt = "error> ".red().to_string();
                        println!("{}", format_error(err));
                    }
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

    Ok(())
}

fn file(args: File) -> Result<(), std::io::Error> {
    let repl_env = env_core();
    prelude(&repl_env, std::env::args());

    let file = fs::read_to_string(args.source)?;
    println!("{:?}", rep(file, &repl_env));
    Ok(())
}

fn main() -> Result<(), std::io::Error> {

    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Repl(args) => {repl(args)},
        SubCommand::File(args) => {file(args)},
    }
}
