use std::env;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Suite {
    tests: Vec<Test>,
    name: String,
}

#[derive(Serialize, Deserialize)]
struct Test {
    id: u32,
    input: String,
    output: String,
    name: String,
    enabled: Option<bool>,
    err: Option<String>,
}

// build script's entry point
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(destination).unwrap();

    // write test file header, put `use`, `const` etc there
    write_header(&mut test_file);

    let test_data_directories = read_dir("./tests/specs/").unwrap();

    for directory in test_data_directories {
        write_test(&mut test_file, &directory.unwrap());
    }
}

fn write_test(test_file: &mut File, directory: &DirEntry) {
    let directory = directory.path().canonicalize().unwrap();

    let source = std::fs::read_to_string(dbg!(directory.clone())).unwrap();
    let suite: Suite = serde_json::from_str(&source).unwrap();

    for test in suite.tests {
        if test.enabled.is_some() && !test.enabled.unwrap() {
            continue;
        }
        let test_name = format!("{}_{}_{}", suite.name, test.name, test.id,);

        write!(
            test_file,
            include_str!("./tests/spec_test_template"),
            name = test_name,
            path = directory.display(),
            input = test.input,
            output = test.output,
        )
        .unwrap();
    }
}

fn write_header(test_file: &mut File) {
    write!(
        test_file,
        r#"
use flang::rep;
use flang::prelude;
"#
    )
    .unwrap();
}
