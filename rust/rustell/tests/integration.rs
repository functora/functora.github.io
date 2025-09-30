use rustell::*;

#[test]
fn test_parser() {
    let src = "use std::io::Read;";
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Many(vec![
            UseExpr::Item {
                module: "io",
                rename: None,
                nested: Some(Box::new(UseExpr::Item {
                    module: "Read",
                    rename: None,
                    nested: None,
                })),
            },
            UseExpr::Item {
                module: "fs",
                rename: None,
                nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Item {
            module: "io",
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
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Item {
            module: "io",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "std",
        rename: None,
        nested: Some(Box::new(UseExpr::Many(vec![
            UseExpr::Item {
                module: "io",
                rename: None,
                nested: Some(Box::new(UseExpr::Item {
                    module: "Read",
                    rename: Some("Readable"),
                    nested: None,
                })),
            },
            UseExpr::Item {
                module: "fs",
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
    let rhs = vec![Expr::Use(UseExpr::Item {
        module: "crate",
        rename: None,
        nested: Some(Box::new(UseExpr::Item {
            module: "module",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Other(
            r#"
    fn test() {
        println!("Hello");
    }"#,
        ),
        Expr::Use(UseExpr::Item {
            module: "crate",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
                module: "module",
                rename: None,
                nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Use(UseExpr::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Use(UseExpr::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Use(UseExpr::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
        Expr::Use(UseExpr::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
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
    let lhs = parser().parse(src).into_result().unwrap();
    let rhs = vec![
        Expr::Use(UseExpr::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(UseExpr::Many(vec![
                UseExpr::Item {
                    module: "io",
                    rename: None,
                    nested: Some(Box::new(UseExpr::Many(
                        vec![
                            UseExpr::Item {
                                module: "self",
                                rename: None,
                                nested: None,
                            },
                            UseExpr::Item {
                                module: "Read",
                                rename: Some("R"),
                                nested: None,
                            },
                        ],
                    ))),
                },
                UseExpr::Item {
                    module: "fs",
                    rename: None,
                    nested: Some(Box::new(UseExpr::Glob)),
                },
            ]))),
        }),
        Expr::Use(UseExpr::Item {
            module: "crate",
            rename: None,
            nested: Some(Box::new(UseExpr::Item {
                module: "module",
                rename: None,
                nested: Some(Box::new(UseExpr::Item {
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
