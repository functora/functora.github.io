use rustell::*;

#[test]
fn test_parser() {
    let src = "use std::io::Read;";
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(ExprUse::Item {
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
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_many() {
    let src = "use std::{io::Read, fs::File};";
    let lhs = expr().parse(src).into_result().unwrap();
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
    assert_eq!(lhs, rhs);
}

#[test]
fn test_parser_glob() {
    let src = "use std::io::*;";
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(ExprUse::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(ExprUse::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(ExprUse::Glob)),
        })),
    })];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_rename() {
    let src = "use std::io::Read as Readable;";
    let lhs = expr().parse(src).into_result().unwrap();
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
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_complex() {
    let src = "use std::{io::Read as Readable, fs::*};";
    let lhs = expr().parse(src).into_result().unwrap();
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
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_crate() {
    let src = "use crate::module::Type;";
    let lhs = expr().parse(src).into_result().unwrap();
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
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_other_then_use() {
    let src = r#"
    fn test() {
        println!("Hello");
    }
    use crate::module::Type;"#;
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Other(
            r#"
    fn test() {
        println!("Hello");
    }"#,
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
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_multiple() {
    let src = r#"
    use std::io;
    use std::fs;
    "#;
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "fs",
                rename: None,
                nested: None,
            })),
        }),
    ];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_multiple_with_other() {
    let src = r#"
    use std::io;
    fn test() {
        println!("Hello");
    }
    use std::fs;
    "#;
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![
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
            r#"fn test() {
        println!("Hello");
    }"#,
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
    ];
    assert_eq!(lhs, rhs)
}

#[test]
fn test_parser_mixed_all_cases() {
    let src = r#"
    use std::{
        io::{self, Read as R},
        fs::*,
    };
    use crate::module::Type as T;

    fn hello() {
        println!("Hello");
    }
    "#;
    let lhs = expr().parse(src).into_result().unwrap();
    let rhs = vec![
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
            r#"fn hello() {
        println!("Hello");
    }
    "#,
        ),
    ];
    assert_eq!(lhs, rhs)
}
