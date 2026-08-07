use rustell::decode;
use rustell::encode;
use rustell::*;
use std::io::{self, Read};

fn main() {
    let mut src = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut src) {
        eprintln!("error reading stdin: {e}");
        std::process::exit(1);
    }
    match decode::expr().parse(src.trim()).into_result() {
        Ok(ast) => {
            for x in encode::expr(&ast) {
                print!("{x}")
            }
        }
        Err(errs) => {
            for e in errs {
                println!("{e:?}")
            }
        }
    }
}
