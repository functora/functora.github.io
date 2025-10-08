use super::*;
use chumsky::prelude::Parser;
use chumsky::prelude::*;
use chumsky::text::whitespace;

pub fn expr<'a>() -> impl Parser<
    'a,
    &'a str,
    Vec<Expr<'a>>,
    extra::Err<Rich<'a, char>>,
> {
    choice((expr_use(), expr_other()))
        .repeated()
        .collect::<Vec<_>>()
}

fn expr_use<'a>()
-> impl Parser<'a, &'a str, Expr<'a>, extra::Err<Rich<'a, char>>>
{
    just("use")
        .ignore_then(expr_use_rec())
        .then_ignore(lexeme(";").or_not())
        .map(Expr::Use)
}

fn expr_use_rec<'a>() -> impl Parser<
    'a,
    &'a str,
    ExprUse<'a>,
    extra::Err<Rich<'a, char>>,
> {
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

fn expr_use_tok<'a>()
-> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>>
+ Clone {
    token(text::ascii::ident()).and_is(
        keyword_except(&["crate", "super", "self", "Self"])
            .not(),
    )
}

fn expr_other<'a>()
-> impl Parser<'a, &'a str, Expr<'a>, extra::Err<Rich<'a, char>>>
{
    any()
        .and_is(expr_use().not())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Expr::Other)
}

fn token<'a>(
    tok: impl Parser<
        'a,
        &'a str,
        &'a str,
        extra::Err<Rich<'a, char>>,
    > + Clone,
) -> impl Parser<
    'a,
    &'a str,
    &'a str,
    extra::Err<Rich<'a, char>>,
> + Clone {
    whitespace().or_not().ignore_then(tok)
}

fn lexeme<'a>(
    seq: &'a str,
) -> impl Parser<
    'a,
    &'a str,
    &'a str,
    extra::Err<Rich<'a, char>>,
> + Clone {
    token(just(seq))
}

fn keyword_except<'a>(
    except: &[&str],
) -> impl Parser<
    'a,
    &'a str,
    &'a str,
    extra::Err<Rich<'a, char>>,
> + Clone {
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
