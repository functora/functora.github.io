pub mod decode;
pub use chumsky::prelude::Parser;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expr<'src> {
    Use(ExprUse<'src>),
    Other(&'src str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ExprUse<'src> {
    Item {
        module: &'src str,
        rename: Option<&'src str>,
        nested: Option<Box<ExprUse<'src>>>,
    },
    Many(Vec<ExprUse<'src>>),
    Glob,
}
