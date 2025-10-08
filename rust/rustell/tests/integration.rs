use rustell::decode;
use rustell::encode;
use rustell::*;

#[test]
fn test_parser() {
    let lhs = "use std::io::Read;";
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn test_parser_raw_then_use() {
    let lhs = r#"
    fn test() {
        println!("Hello")
    }
    use crate::module::Type;"#;
    let rhs = vec![
        Expr::Raw(
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
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn test_parser_multiple() {
    let lhs = r#"
    use std::io;
    use std::fs;
    "#;
    let rhs = vec![
        Expr::Raw("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Raw("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "fs",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Raw("\n    "),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn test_parser_multiple_with_raw() {
    let lhs = r#"
    use std::io;
    fn test() {
        println!("Hello")
    }
    use std::fs;
    "#;
    let rhs = vec![
        Expr::Raw("\n    "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
        Expr::Raw(
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
        Expr::Raw("\n    "),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
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
        Expr::Raw("\n    "),
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
        Expr::Raw("\n    "),
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
        Expr::Raw(
            r#"

    fn hello() {
        println!("Hello")
    }
    "#,
        ),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn test_rountrip_lib() {
    let src =
        std::fs::read_to_string("./src/lib.rs").unwrap();
    let ast = decode(&src);
    assert_eq!(decode(&encode(&ast)), ast);
}

fn sloppy(src: &str) -> String {
    src.replace(";", "")
}

fn decode(src: &str) -> Vec<Expr> {
    decode::expr().parse(src).into_result().unwrap()
}

fn encode(ast: &[Expr]) -> String {
    encode::expr(ast).collect()
}
