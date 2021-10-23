extern crate exitcode;
extern crate colored;

use colored::*;
use serde::{Serialize, Deserialize};

use flang::mal::rep;
use flang::core::env_core;

#[derive(Serialize, Deserialize)]
struct Test {
    input: String,
    result: String,
}

fn main() -> Result<(), ()>
{
    let args: Vec<String> = std::env::args().collect();
    let tests = std::fs::read_to_string(args[1].clone())
        .expect("Something went wrong reading the file");

    let mut passed: u32 = 0;
    let tests: Vec<Test> = serde_json::from_str::<Vec<Test>>(&tests).unwrap();
    for test in &tests {
        let repl_env = env_core();
        match rep(test.input.clone(), &repl_env) {
            Ok(r) => {
                if r == test.result {
                    passed += 1;
                    println!("{}", ".".green());
                }
                else {
                    println!("Failed: {} -> {}. Got: {}", test.input, test.result, r);
                    continue;
                }
            },
            Err(err) => {
                println!("Failed {:?}", err);
                continue;
            }
        };
    }

    println!("{}", std::iter::repeat("-").take(80).collect::<String>());
    let blank = std::iter::repeat(" ").take(77).collect::<String>();
    println!("{}{}/{}", blank, passed, tests.len());

    if passed == tests.len() as u32 {
        Ok(())
    }
    else {
        Err(())
    }
}

