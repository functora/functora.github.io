use rustell::decode;
use rustell::encode;
use rustell::*;
use std::io::{self, Read};

fn main() {
    let mut src = String::new();
    io::stdin()
        .read_to_string(&mut src)
        .expect("Failed to read stdin");
    match decode::expr().parse(src.trim()).into_result() {
        Ok(ast) => {
            encode::expr(ast).for_each(|x| print!("{}", x))
        }
        Err(errs) => {
            errs.into_iter().for_each(|e| println!("{e:?}"))
        }
    };
}
