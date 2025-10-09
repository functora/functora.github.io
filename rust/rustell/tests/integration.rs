use rustell::decode;
use rustell::encode;
use rustell::*;

#[test]
fn mod_declaration() {
    let lhs = "mod hello;";
    let rhs = vec![Expr::Mod("hello")];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn mod_then_use_statement() {
    let lhs = "mod hello; use std::io;";
    let rhs = vec![
        Expr::Mod("hello"),
        Expr::Raw(" "),
        Expr::Use(ExprUse::Item {
            module: "std",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "io",
                rename: None,
                nested: None,
            })),
        }),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn raw_code_then_mod() {
    let lhs = r#"
    fn test() {
        println!("Hello")
    }
    mod hello;"#;
    let rhs = vec![
        Expr::Raw(
            r#"
    fn test() {
        println!("Hello")
    }
    "#,
        ),
        Expr::Mod("hello"),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn mod_then_raw_code() {
    let lhs = "mod hello;
fn test() {
    println!(\"Hello\")
}";
    let rhs = vec![
        Expr::Mod("hello"),
        Expr::Raw(
            "
fn test() {
    println!(\"Hello\")
}",
        ),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn simple_use_statement() {
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
fn multiple_use_items() {
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
fn use_glob_pattern() {
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
fn use_with_rename() {
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
fn complex_use_statement() {
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
fn use_crate_path() {
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
fn raw_code_then_use() {
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
fn multiple_separate_uses() {
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
fn multiple_uses_with_raw_code() {
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
fn mixed_use_and_raw_cases() {
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
fn roundtrip_source_files() {
    get_rust_files("./").into_iter().for_each(|path| {
        let lhs = std::fs::read_to_string(&path).unwrap();
        let rhs = decode(&lhs);
        assert_eq!(
            decode(&encode(&rhs)),
            rhs,
            "Roundtrip failed for the file: {}",
            path
        );
    });
}

fn get_rust_files(dir: &str) -> Vec<String> {
    std::fs::read_dir(dir)
        .unwrap()
        .flat_map(|item| {
            let path = item.unwrap().path();
            let name = path.to_str().unwrap();
            if path.is_dir() {
                get_rust_files(name)
            } else if path.is_file()
                && name.ends_with(".rs")
            {
                vec![name.to_string()]
            } else {
                vec![]
            }
        })
        .collect()
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
