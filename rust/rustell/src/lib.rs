use chumsky::container::OrderedSeq;
use chumsky::extra::ParserExtra;
pub use chumsky::prelude::Parser;
use chumsky::prelude::*;
use chumsky::primitive::Just;
use chumsky::text::{Char, Padded};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expr<'src> {
    Use(UseExpr<'src>),
    Other(&'src str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum UseExpr<'src> {
    Item {
        module: &'src str,
        rename: Option<&'src str>,
        nested: Option<Box<UseExpr<'src>>>,
    },
    Many(Vec<UseExpr<'src>>),
    Glob,
}

pub fn parser<'src>() -> impl Parser<
    'src,
    &'src str,
    Vec<Expr<'src>>,
    extra::Err<Rich<'src, char>>,
> {
    let use_expr = || {
        lexeme("use")
            .ignore_then(use_parser())
            .then_ignore(lexeme(";").or_not())
            .map(Expr::Use)
    };

    let other = use_expr()
        .not()
        .ignore_then(any())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Expr::Other);

    use_expr().or(other).repeated().collect::<Vec<_>>()
}

fn use_parser<'src>() -> impl Parser<
    'src,
    &'src str,
    UseExpr<'src>,
    extra::Err<Rich<'src, char>>,
> {
    recursive(|use_parser| {
        let ident = || text::ascii::ident().padded();
        let item = ident()
            .then(
                lexeme("as").ignore_then(ident()).or_not(),
            )
            .then(
                lexeme("::")
                    .ignore_then(use_parser.clone())
                    .or_not(),
            )
            .map(|((module, rename), nested)| {
                UseExpr::Item {
                    module,
                    rename,
                    nested: nested.map(Box::new),
                }
            });

        let many = use_parser
            .separated_by(lexeme(','))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(lexeme('{'), lexeme('}'))
            .map(UseExpr::Many);

        let glob = lexeme("*").map(|_| UseExpr::Glob);

        item.or(many).or(glob)
    })
}

fn lexeme<'src, T, I, E>(seq: T) -> Padded<Just<T, I, E>>
where
    I: Input<'src>,
    I::Token: Char,
    E: ParserExtra<'src, I>,
    T: OrderedSeq<'src, I::Token> + Clone,
{
    just(seq).padded()
}
