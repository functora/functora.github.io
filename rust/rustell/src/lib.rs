pub mod decode;
pub mod encode;
pub use chumsky::prelude::Parser;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    Mod(&'a str),
    Use(ExprUse<'a>),
    Raw(&'a str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ExprUse<'a> {
    Item {
        module: &'a str,
        rename: Option<&'a str>,
        nested: Option<Box<ExprUse<'a>>>,
    },
    Many(Vec<ExprUse<'a>>),
    Glob,
}
