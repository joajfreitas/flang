extern crate exitcode;

use serde::{Serialize, Deserialize};

use flang::mal::rep;
use flang::core::env_core;

#[derive(Serialize, Deserialize)]
struct Test {
    input: String,
    result: String,
}

fn main()
{
    let args: Vec<String> = std::env::args().collect();
    let tests = std::fs::read_to_string(args[1].clone())
        .expect("Something went wrong reading the file");

    let mut passed: u32 = 0;
    let tests: Vec<Test> = serde_json::from_str::<Vec<Test>>(&tests)?;
    for test in &tests {
        let repl_env = env_core();
        match rep(test.input.clone(), &repl_env) {
            Ok(r) => {
                if r == test.result {
                    passed += 1;
                    println!(".");
                }
                else {
                    println!("Failed: {} -> {}", test.input, test.result);
                    continue;
                }
            },
            Err(err) => {
                println!("Failed");
                continue;
            }
        };
    }
    println!("------------------------------------------------------------------------------------------------------");
    println!("{}/{}", passed, tests.len());

    if passed == tests.len() as u32 {
        std::process::exit(exitcode::OK);
    }
    else {
        std::proess:exit()
    }
}

