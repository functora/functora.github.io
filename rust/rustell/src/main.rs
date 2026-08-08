use rustell::decode;
use rustell::encode;
use rustell::*;
use std::io;

fn main() {
    let src = io::read_to_string(io::stdin())
        .unwrap_or_else(|e| {
            eprintln!("error reading stdin: {e}");
            std::process::exit(1);
        });
    match decode::expr().parse(src.trim()).into_result() {
        Ok(ast) => {
            encode::expr(&ast).for_each(|x| print!("{x}"))
        }
        Err(errs) => {
            for e in errs {
                println!("{e:?}")
            }
        }
    }
}
