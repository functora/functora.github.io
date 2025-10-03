use rustell::decode;
use rustell::encode;
use rustell::*;

#[test]
fn test_parser() {
    let src = "use std::io::Read;";
    let slp = sloppy(src);
    let ast = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "Read",
                rename: None,
                nested: None,
            })),
        })),
    })];
    assert_eq!(parse(src), ast);
    assert_eq!(parse(&slp), ast);
    assert_eq!(parse(&encode(ast.clone())), ast)
}

#[test]
fn test_parser_many() {
    let lhs = "use std::{io::Read, fs::File};";
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Many(vec![
            ExprUse::Item {
                module: "io",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "Read",
                    rename: None,
                    nested: None,
                })),
            },
            ExprUse::Item {
                module: "fs",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "File",
                    rename: None,
                    nested: None,
                })),
            },
        ]))),
    })];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_glob() {
    let lhs = "use std::io::*;";
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(ExprUse::Glob)),
        })),
    })];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_rename() {
    let lhs = "use std::io::Read as Readable;";
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "Read",
                rename: Some("Readable"),
                nested: None,
            })),
        })),
    })];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_complex() {
    let lhs = "use std::{io::Read as Readable, fs::*};";
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Many(vec![
            ExprUse::Item {
                module: "io",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "Read",
                    rename: Some("Readable"),
                    nested: None,
                })),
            },
            ExprUse::Item {
                module: "fs",
                rename: None,
                nested: Some(Box::new(ExprUse::Glob)),
            },
        ]))),
    })];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_crate() {
    let lhs = "use crate::module::Type;";
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "crate",
        rename: None,
        nested: Some(Box::new(ExprUse::Item {
            module: "module",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "Type",
                rename: None,
                nested: None,
            })),
        })),
    })];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_other_then_use() {
    let lhs = r#"
    fn test() {
        println!("Hello")
    }
    use crate::module::Type;"#;
    let rhs = vec![
        Expr::Other(
            r#"
    fn test() {
        println!("Hello")
    }
    "#,
        ),
        Expr::Use(ExprUse::Item {
            module: "crate",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "module",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "Type",
                    rename: None,
                    nested: None,
                })),
            })),
        }),
    ];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_multiple() {
    let lhs = r#"
    use std::io;
    use std::fs;
    "#;
    let rhs = vec![
        Expr::Other("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Other("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "fs",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Other("\n    "),
    ];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_multiple_with_other() {
    let lhs = r#"
    use std::io;
    fn test() {
        println!("Hello")
    }
    use std::fs;
    "#;
    let rhs = vec![
        Expr::Other("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Other(
            r#"
    fn test() {
        println!("Hello")
    }
    "#,
        ),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "fs",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Other("\n    "),
    ];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

#[test]
fn test_parser_mixed_all_cases() {
    let lhs = r#"
    use std::{
        io::{self, Read as R},
        fs::*,
    };
    use crate::module::Type as T;

    fn hello() {
        println!("Hello")
    }
    "#;
    let rhs = vec![
        Expr::Other("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Many(vec![
                ExprUse::Item {
                    module: "io",
                    rename: None,
                    nested: Some(Box::new(ExprUse::Many(
                        vec![
                            ExprUse::Item {
                                module: "self",
                                rename: None,
                                nested: None,
                            },
                            ExprUse::Item {
                                module: "Read",
                                rename: Some("R"),
                                nested: None,
                            },
                        ],
                    ))),
                },
                ExprUse::Item {
                    module: "fs",
                    rename: None,
                    nested: Some(Box::new(ExprUse::Glob)),
                },
            ]))),
        }),
        Expr::Other("\n    "),
        Expr::Use(ExprUse::Item {
            module: "crate",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "module",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "Type",
                    rename: Some("T"),
                    nested: None,
                })),
            })),
        }),
        Expr::Other(
            r#"

    fn hello() {
        println!("Hello")
    }
    "#,
        ),
    ];
    assert_eq!(parse(lhs), rhs);
    assert_eq!(parse(&sloppy(lhs)), rhs)
}

fn sloppy(src: &str) -> String {
    src.replace(";", "")
}

fn parse(src: &str) -> Vec<Expr> {
    decode::expr().parse(src).into_result().unwrap()
}

fn encode(ast: Vec<Expr>) -> String {
    encode::expr(ast).collect()
}
