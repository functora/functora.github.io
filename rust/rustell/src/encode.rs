use super::*;
use std::iter::{IntoIterator, once};
use std::vec::IntoIter;

pub fn expr<'a>(
    ast: Vec<Expr<'a>>,
) -> impl Iterator<Item = &'a str> {
    ast.into_iter().flat_map(expr_one)
}

fn expr_one<'a>(ast: Expr<'a>) -> IntoIter<&'a str> {
    match ast {
        Expr::Use(x) => expr_use(true, x)
            .chain(vec![";"].into_iter())
            .collect::<Vec<&'a str>>()
            .into_iter(),
        Expr::Other(x) => vec![x].into_iter(),
    }
}

fn expr_use<'a>(
    top: bool,
    ast: ExprUse<'a>,
) -> IntoIter<&'a str> {
    let x0 = if top { vec!["use"] } else { vec![] };
    let x1 = match ast {
        ExprUse::Item {
            module,
            rename,
            nested,
        } => expr_use_item(module, rename, nested),
        ExprUse::Many(xs) => expr_use_many(xs),
        ExprUse::Glob => vec!["*"].into_iter(),
    };
    x0.into_iter()
        .chain(x1.into_iter())
        .collect::<Vec<_>>()
        .into_iter()
}

fn expr_use_item<'a>(
    module: &'a str,
    rename: Option<&'a str>,
    nested: Option<Box<ExprUse<'a>>>,
) -> IntoIter<&'a str> {
    let module_iter = once(module);

    let rename_iter = rename
        .map(|x| once("as").chain(once(x)))
        .into_iter()
        .flatten();

    let nested_iter = nested
        .map(|x| once("::").chain(expr_use(false, *x)))
        .into_iter()
        .flatten();

    module_iter
        .chain(rename_iter)
        .chain(nested_iter)
        .collect::<Vec<_>>()
        .into_iter()
}

fn expr_use_many<'a>(
    xs: Vec<ExprUse<'a>>,
) -> IntoIter<&'a str> {
    xs.into_iter()
        .flat_map(|x| expr_use(false, x))
        .collect::<Vec<_>>()
        .into_iter()
}
