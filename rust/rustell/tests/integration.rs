use rustell::*;

#[test]
fn test_parser() {
    let src = "use std::io::Read;";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Path {
            ident: "io",
            rename: None,
            nested: Some(Box::new(UseExpr::Path {
                ident: "Read",
                rename: None,
                nested: None,
            })),
        })),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_group() {
    let src = "use std::{io::Read, fs::File};";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Group(vec![
            UseExpr::Path {
                ident: "io",
                rename: None,
                nested: Some(Box::new(UseExpr::Path {
                    ident: "Read",
                    rename: None,
                    nested: None,
                })),
            },
            UseExpr::Path {
                ident: "fs",
                rename: None,
                nested: Some(Box::new(UseExpr::Path {
                    ident: "File",
                    rename: None,
                    nested: None,
                })),
            },
        ]))),
    })];
    assert_eq!(lhs, rhs);
}

#[test]
fn test_parser_glob() {
    let src = "use std::io::*;";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Path {
            ident: "io",
            rename: None,
            nested: Some(Box::new(UseExpr::Glob)),
        })),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_rename() {
    let src = "use std::io::Read as Readable;";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Path {
            ident: "io",
            rename: None,
            nested: Some(Box::new(UseExpr::Path {
                ident: "Read",
                rename: Some("Readable"),
                nested: None,
            })),
        })),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_complex() {
    let src = "use std::{io::Read as Readable, fs::*};";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Group(vec![
            UseExpr::Path {
                ident: "io",
                rename: None,
                nested: Some(Box::new(UseExpr::Path {
                    ident: "Read",
                    rename: Some("Readable"),
                    nested: None,
                })),
            },
            UseExpr::Path {
                ident: "fs",
                rename: None,
                nested: Some(Box::new(UseExpr::Glob)),
            },
        ]))),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_crate() {
    let src = "use crate::module::Type;";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Path {
        ident: "crate",
        rename: None,
        nested: Some(Box::new(UseExpr::Path {
            ident: "module",
            rename: None,
            nested: Some(Box::new(UseExpr::Path {
                ident: "Type",
                rename: None,
                nested: None,
            })),
        })),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_other_then_use() {
    let src = r#"
    fn test() {
        println!("Hello");
    }
    use crate::module::Type;"#;
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Other(
            r#"
    fn test() {
        println!("Hello");
    }"#,
        ),
        Expr::Use(UseExpr::Path {
            ident: "crate",
            rename: None,
            nested: Some(Box::new(UseExpr::Path {
                ident: "module",
                rename: None,
                nested: Some(Box::new(UseExpr::Path {
                    ident: "Type",
                    rename: None,
                    nested: None,
                })),
            })),
        }),
    ];
    assert_eq!(lhs, rhs)
}
