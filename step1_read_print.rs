extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
mod types;
mod reader;
mod printer;

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".flang-history").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                rl.save_history(".flang-history").unwrap();
                match reader::read_str(line) {
                    Some(mv) => {
                        println!("{}", mv.pr_str());
                    },
                    None => println!("Error printing AST"),
                };
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
