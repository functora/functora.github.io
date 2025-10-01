pub use chumsky::prelude::Parser;
use chumsky::prelude::*;
use chumsky::text::whitespace;

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

pub fn expr<'src>() -> impl Parser<
    'src,
    &'src str,
    Vec<Expr<'src>>,
    extra::Err<Rich<'src, char>>,
> {
    expr_use()
        .or(expr_other())
        .repeated()
        .collect::<Vec<_>>()
}

fn expr_use<'src>() -> impl Parser<
    'src,
    &'src str,
    Expr<'src>,
    extra::Err<Rich<'src, char>>,
> {
    just("use")
        .ignore_then(expr_use_rec())
        .then_ignore(lexeme(";").or_not())
        .map(Expr::Use)
}

fn expr_use_rec<'src>() -> impl Parser<
    'src,
    &'src str,
    ExprUse<'src>,
    extra::Err<Rich<'src, char>>,
> {
    recursive(|expr_use| {
        let ident = || text::ascii::ident().padded();
        let item = ident()
            .then(
                lexeme("as").ignore_then(ident()).or_not(),
            )
            .then(
                lexeme("::")
                    .ignore_then(expr_use.clone())
                    .or_not(),
            )
            .map(|((module, rename), nested)| {
                ExprUse::Item {
                    module,
                    rename,
                    nested: nested.map(Box::new),
                }
            });

        let many = expr_use
            .separated_by(lexeme(","))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(lexeme("{"), lexeme("}"))
            .map(ExprUse::Many);

        let glob = lexeme("*").map(|_| ExprUse::Glob);

        item.or(many).or(glob)
    })
}

fn expr_other<'src>() -> impl Parser<
    'src,
    &'src str,
    Expr<'src>,
    extra::Err<Rich<'src, char>>,
> {
    expr_use()
        .not()
        .ignore_then(any())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Expr::Other)
}

fn lexeme<'src>(
    seq: &'static str,
) -> impl Parser<
    'src,
    &'src str,
    &'src str,
    extra::Err<Rich<'src, char>>,
> + Clone {
    whitespace().or_not().ignore_then(just(seq))
}
