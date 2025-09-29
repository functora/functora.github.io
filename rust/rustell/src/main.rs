use chumsky::prelude::*;
use std::io::{self, Read};

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr,
    Loop(Vec<Stmt>),
}

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Stmt>> {
    let expr = just("expr"); // TODO

    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));

        let expr_stmt = expr.then_ignore(text::newline()).to(Stmt::Expr);
        let control_flow = just("loop:")
            .then(text::newline())
            .ignore_then(block)
            .map(Stmt::Loop);
        let stmt = expr_stmt.or(control_flow);

        text::whitespace()
            .count()
            .ignore_with_ctx(stmt.separated_by(indent).collect())
    });

    block.with_ctx(0)
}

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
