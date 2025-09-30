use rustell::*;
use std::io::{self, Read};

fn main() {
    let mut src = String::new();
    io::stdin()
        .read_to_string(&mut src)
        .expect("Failed to read stdin");
    match parser().parse(src.trim()).into_result() {
        Ok(ast) => println!("{:#?}", ast),
        Err(errs) => errs.into_iter().for_each(|e| println!("{e:?}")),
    };
}
