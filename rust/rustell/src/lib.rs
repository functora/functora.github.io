pub mod decode;
pub mod encode;
pub use chumsky::prelude::Parser;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    Mod(&'a str),
    Use(ExprUse<'a>),
    Jump(ExprJump<'a>),
    Block(Vec<Expr<'a>>),
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

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ExprJump<'a> {
    Break,
    Continue,
    Return(Box<Expr<'a>>),
}
