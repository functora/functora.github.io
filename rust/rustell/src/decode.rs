use super::*;
use chumsky::prelude::Parser;
use chumsky::prelude::*;
use chumsky::text::whitespace;

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

fn expr_use_tok<'src>() -> impl Parser<
    'src,
    &'src str,
    &'src str,
    extra::Err<Rich<'src, char>>,
> + Clone {
    token(text::ascii::ident()).and_is(
        keyword_except(&["crate", "super", "self", "Self"])
            .not(),
    )
}

fn expr_other<'src>() -> impl Parser<
    'src,
    &'src str,
    Expr<'src>,
    extra::Err<Rich<'src, char>>,
> {
    any()
        .and_is(expr_use().not())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Expr::Other)
}

fn token<'src>(
    tok: impl Parser<
        'src,
        &'src str,
        &'src str,
        extra::Err<Rich<'src, char>>,
    > + Clone,
) -> impl Parser<
    'src,
    &'src str,
    &'src str,
    extra::Err<Rich<'src, char>>,
> + Clone {
    whitespace().or_not().ignore_then(tok)
}

fn lexeme<'src>(
    seq: &'src str,
) -> impl Parser<
    'src,
    &'src str,
    &'src str,
    extra::Err<Rich<'src, char>>,
> + Clone {
    token(just(seq))
}

fn keyword_except<'src>(
    except: &[&str],
) -> impl Parser<
    'src,
    &'src str,
    &'src str,
    extra::Err<Rich<'src, char>>,
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
