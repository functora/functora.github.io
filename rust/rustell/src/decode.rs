use super::*;
use chumsky::prelude::Parser;
use chumsky::prelude::*;
use chumsky::text::whitespace;

pub trait Decode<'a, T>:
    Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>
{
}

impl<'a, T, U> Decode<'a, T> for U where
    U: Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>
{
}

pub fn expr<'a>() -> impl Decode<'a, Vec<Expr<'a>>> {
    choice((expr_ast(), expr_raw()))
        .repeated()
        .collect::<Vec<_>>()
}

fn expr_ast<'a>() -> impl Decode<'a, Expr<'a>> {
    choice((expr_mod(), expr_use()))
}

fn expr_mod<'a>() -> impl Decode<'a, Expr<'a>> {
    let tok =
        token(text::ascii::ident()).and_is(keyword().not());
    just("mod")
        .ignore_then(tok)
        .then_ignore(lexeme(";").or_not())
        .map(Expr::Mod)
}

fn expr_use<'a>() -> impl Decode<'a, Expr<'a>> {
    just("use")
        .ignore_then(expr_use_rec())
        .then_ignore(lexeme(";").or_not())
        .map(Expr::Use)
}

fn expr_use_rec<'a>() -> impl Decode<'a, ExprUse<'a>> {
    recursive(|expr_use_rec| {
        let item = expr_use_tok()
            .then(
                lexeme("as")
                    .ignore_then(expr_use_tok())
                    .or_not(),
            )
            .then(
                lexeme("::")
                    .ignore_then(expr_use_rec.clone())
                    .or_not(),
            )
            .map(|((module, rename), nested)| {
                ExprUse::Item {
                    module,
                    rename,
                    nested: nested.map(Box::new),
                }
            });

        let many = expr_use_rec
            .separated_by(lexeme(","))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(lexeme("{"), lexeme("}"))
            .map(ExprUse::Many);

        let glob = lexeme("*").map(|_| ExprUse::Glob);

        item.or(many).or(glob)
    })
}

fn expr_use_tok<'a>() -> impl Decode<'a, &'a str> + Clone {
    token(text::ascii::ident()).and_is(
        keyword_except(&["crate", "super", "self", "Self"])
            .not(),
    )
}

fn expr_raw<'a>() -> impl Decode<'a, Expr<'a>> {
    any()
        .and_is(expr_ast().not())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Expr::Raw)
}

fn token<'a>(
    tok: impl Decode<'a, &'a str> + Clone,
) -> impl Decode<'a, &'a str> + Clone {
    whitespace().or_not().ignore_then(tok)
}

fn lexeme<'a>(
    seq: &'a str,
) -> impl Decode<'a, &'a str> + Clone {
    token(just(seq))
}

fn keyword<'a>() -> impl Decode<'a, &'a str> + Clone {
    keyword_except(&[])
}

fn keyword_except<'a>(
    except: &[&str],
) -> impl Decode<'a, &'a str> + Clone {
    choice(
        [
            "as", "break", "const", "continue", "crate",
            "else", "enum", "extern", "false", "fn", "for",
            "if", "impl", "in", "let", "loop", "match",
            "mod", "move", "mut", "pub", "ref", "return",
            "self", "Self", "static", "struct", "super",
            "trait", "true", "type", "unsafe", "use",
            "where", "while", "async", "await", "dyn",
            "abstract", "become", "box", "do", "final",
            "macro", "override", "priv", "typeof",
            "unsized", "virtual", "yield", "try", "gen",
        ]
        .iter()
        .filter(|x| !except.contains(x))
        .map(|&x| {
            lexeme(x)
                .then_ignore(text::ascii::ident().not())
        })
        .collect::<Vec<_>>(),
    )
}
