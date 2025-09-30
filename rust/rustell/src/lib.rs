pub use chumsky::prelude::Parser;
use chumsky::prelude::*;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Expr<'src> {
    Use(UseExpr<'src>),
    Other(&'src str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum UseExpr<'src> {
    Path {
        ident: &'src str,
        rename: Option<&'src str>,
        nested: Option<Box<UseExpr<'src>>>,
    },
    Group(Vec<UseExpr<'src>>),
    Glob,
}

pub fn parser<'src>()
-> impl Parser<'src, &'src str, Vec<Expr<'src>>, extra::Err<Rich<'src, char>>> {
    let use_expr = || {
        just("use")
            .padded()
            .ignore_then(use_parser())
            .then_ignore(just(";").padded().or_not())
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

fn use_parser<'src>()
-> impl Parser<'src, &'src str, UseExpr<'src>, extra::Err<Rich<'src, char>>> {
    recursive(|use_parser| {
        let ident = || text::ascii::ident().padded();
        let path = ident()
            .then(just("as").padded().ignore_then(ident()).or_not())
            .then(just("::").padded().ignore_then(use_parser.clone()).or_not())
            .map(|((ident, rename), nested)| UseExpr::Path {
                ident,
                rename,
                nested: nested.map(Box::new),
            });

        let group = use_parser
            .separated_by(just(','))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(UseExpr::Group);

        let glob = just("*").padded().map(|_| UseExpr::Glob);

        path.or(group).or(glob)
    })
}
