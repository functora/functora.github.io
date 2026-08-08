#![allow(clippy::unwrap_used, clippy::expect_used)]
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
    let lhs = r"
    use std::io;
    use std::fs;
    ";
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
fn small_mixed_lib() {
    let lhs = r"
    pub mod decode;
    pub mod encode;
    pub use chumsky::prelude::Parser;

    #[derive(Eq, PartialEq, Debug, Clone)]
    pub enum Expr<'a> {
        Mod(&'a str),
        Use(ExprUse<'a>),
        Raw(&'a str),
    }

    #[derive(Eq, PartialEq, Debug, Clone)]
    pub enum ExprUse<'a> {
        Item {
            module: &'a str,
            rename: Option<&'a str>,
            nested: Option<Box<ExprUse<'a>>>,
        },
        Many(Vec<ExprUse<'a>>),
        Glob,
    }";
    let rhs = vec![
        Expr::Raw("\n    pub "),
        Expr::Mod("decode"),
        Expr::Raw("\n    pub "),
        Expr::Mod("encode"),
        Expr::Raw("\n    pub "),
        Expr::Use(ExprUse::Item {
            module: "chumsky",
            rename: None,
            nested: Some(Box::new(ExprUse::Item {
                module: "prelude",
                rename: None,
                nested: Some(Box::new(ExprUse::Item {
                    module: "Parser",
                    rename: None,
                    nested: None,
                })),
            })),
        }),
        Expr::Raw(
            r"

    #[derive(Eq, PartialEq, Debug, Clone)]
    pub enum Expr<'a> {
        Mod(&'a str),
        Use(ExprUse<'a>),
        Raw(&'a str),
    }

    #[derive(Eq, PartialEq, Debug, Clone)]
    pub enum ExprUse<'a> {
        Item {
            module: &'a str,
            rename: Option<&'a str>,
            nested: Option<Box<ExprUse<'a>>>,
        },
        Many(Vec<ExprUse<'a>>),
        Glob,
    }",
        ),
    ];
    assert_eq!(decode(lhs), rhs);
    assert_eq!(decode(&sloppy(lhs)), rhs);
    assert_eq!(decode(&encode(&rhs)), rhs)
}

#[test]
fn roundtrip_source_files() {
    for path in get_rust_files("./") {
        let lhs = std::fs::read_to_string(&path).unwrap();
        let rhs = decode(&lhs);
        assert_eq!(
            decode(&encode(&rhs)),
            rhs,
            "Roundtrip failed for the file: {path}"
        );
    }
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
                && path
                    .extension()
                    .is_some_and(|e| e == "rs")
            {
                vec![name.to_string()]
            } else {
                vec![]
            }
        })
        .collect()
}

fn sloppy(src: &str) -> String {
    src.replace(';', "")
}

fn decode(src: &str) -> Vec<Expr<'_>> {
    decode::expr().parse(src).into_result().unwrap()
}

fn encode(ast: &[Expr]) -> String {
    encode::expr(ast).collect()
}

//
// Deterministic property-based round-trip (no external dependencies).
//

enum Spec {
    Mod(usize),
    Use(UseSpec),
    Jump(JumpSpec),
    Block(Vec<Spec>),
    Raw(&'static str),
}

enum UseSpec {
    Item {
        module: usize,
        rename: Option<usize>,
        nested: Option<Box<UseSpec>>,
    },
    Many(Vec<UseSpec>),
    Glob,
}

enum JumpSpec {
    Break,
    Continue,
    Return(Box<Spec>),
}

const IDENT_BASE: &[&str] = &[
    "alpha", "beta", "gamma", "delta", "eps", "zeta",
    "eta", "theta", "iota", "kappa",
];

const SAFE_RAWS: &[&str] = &[
    "x",
    "y",
    "z",
    "let",
    "let a = 1",
    "while true",
    "match",
    "loop",
    "for x in y",
    "if a { b }",
    "struct",
    "enum",
    "impl",
    "123",
    "fn",
    "->",
    "::",
    "0x1F",
    "'k",
    "|a b|",
    "&&",
    "0b1",
    "a.b",
    "@",
    "sep",
    "_",
];

fn next_u64(state: &mut u64) -> u64 {
    let mut x = *state;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    *state = x;
    x
}

fn pick_mod(state: &mut u64, n: u64) -> u64 {
    next_u64(state) % n
}

fn pick_index(state: &mut u64, len: usize) -> usize {
    if len == 0 {
        0
    } else {
        usize::try_from(next_u64(state))
            .expect("rng fits usize")
            % len
    }
}

fn coin(state: &mut u64) -> bool {
    pick_mod(state, 2) == 0
}

fn pick<'a, T>(state: &mut u64, xs: &'a [T]) -> &'a T {
    xs.get(pick_index(state, xs.len()))
        .expect("non-empty pool")
}

fn gen_idents(
    state: &mut u64,
    count: usize,
) -> Vec<String> {
    (0..count)
        .map(|i| format!("{}{i}", pick(state, IDENT_BASE)))
        .collect()
}

fn gen_program(
    state: &mut u64,
    ident_count: usize,
) -> Vec<Spec> {
    (0..=pick_mod(state, 14))
        .map(|_| gen_expr(state, ident_count, 0))
        .collect()
}

fn gen_expr(
    state: &mut u64,
    ident_count: usize,
    depth: u64,
) -> Spec {
    if depth >= 5 {
        return match pick_mod(state, 10) {
            0..=4 => {
                Spec::Mod(pick_index(state, ident_count))
            }
            _ => Spec::Raw(pick(state, SAFE_RAWS)),
        };
    }
    match pick_mod(state, 100) {
        0..=44 => Spec::Mod(pick_index(state, ident_count)),
        45..=64 => {
            Spec::Use(gen_use(state, ident_count, depth))
        }
        65..=74 => {
            Spec::Jump(gen_jump(state, ident_count, depth))
        }
        75..=84 => Spec::Block(gen_block(
            state,
            ident_count,
            depth,
        )),
        _ => Spec::Raw(pick(state, SAFE_RAWS)),
    }
}

fn gen_use(
    state: &mut u64,
    ident_count: usize,
    depth: u64,
) -> UseSpec {
    if depth >= 4 {
        UseSpec::Item {
            module: pick_index(state, ident_count),
            rename: None,
            nested: None,
        }
    } else {
        match pick_mod(state, 10) {
            0..=4 => UseSpec::Item {
                module: pick_index(state, ident_count),
                rename: coin(state).then(|| {
                    pick_index(state, ident_count)
                }),
                nested: coin(state).then(|| {
                    Box::new(gen_use(
                        state,
                        ident_count,
                        depth + 1,
                    ))
                }),
            },
            5..=8 => UseSpec::Many(gen_use_many(
                state,
                ident_count,
            )),
            _ => UseSpec::Glob,
        }
    }
}

fn gen_use_many(
    state: &mut u64,
    ident_count: usize,
) -> Vec<UseSpec> {
    (0..=pick_index(state, 3))
        .map(|_| UseSpec::Item {
            module: pick_index(state, ident_count),
            rename: coin(state)
                .then(|| pick_index(state, ident_count)),
            nested: None,
        })
        .collect()
}

fn gen_jump(
    state: &mut u64,
    ident_count: usize,
    depth: u64,
) -> JumpSpec {
    match pick_mod(state, 10) {
        0..=3 => JumpSpec::Break,
        4..=6 => JumpSpec::Continue,
        _ => JumpSpec::Return(Box::new(gen_return_child(
            state,
            ident_count,
            depth,
        ))),
    }
}

fn gen_return_child(
    state: &mut u64,
    ident_count: usize,
    depth: u64,
) -> Spec {
    match pick_mod(state, 10) {
        0..=4 => Spec::Mod(pick_index(state, ident_count)),
        5..=8 => {
            Spec::Use(gen_use(state, ident_count, depth))
        }
        _ => {
            Spec::Jump(gen_jump(state, ident_count, depth))
        }
    }
}

fn gen_block(
    state: &mut u64,
    ident_count: usize,
    depth: u64,
) -> Vec<Spec> {
    (0..=pick_index(state, 3))
        .map(|_| gen_expr(state, ident_count, depth))
        .collect()
}

fn materialize<'a>(
    specs: &[Spec],
    idents: &'a [String],
) -> Vec<Expr<'a>> {
    specs
        .iter()
        .map(|s| materialize_expr(s, idents))
        .collect()
}

fn materialize_expr<'a>(
    spec: &Spec,
    idents: &'a [String],
) -> Expr<'a> {
    match spec {
        Spec::Mod(i) => Expr::Mod(ident_str(idents, *i)),
        Spec::Use(u) => {
            Expr::Use(materialize_use(u, idents))
        }
        Spec::Jump(j) => {
            Expr::Jump(materialize_jump(j, idents))
        }
        Spec::Block(xs) => {
            Expr::Block(materialize(xs, idents))
        }
        Spec::Raw(s) => Expr::Raw(s),
    }
}

fn ident_str(idents: &[String], i: usize) -> &str {
    idents.get(i).expect("ident index in range").as_str()
}

fn materialize_jump<'a>(
    spec: &JumpSpec,
    idents: &'a [String],
) -> ExprJump<'a> {
    match spec {
        JumpSpec::Break => ExprJump::Break,
        JumpSpec::Continue => ExprJump::Continue,
        JumpSpec::Return(x) => ExprJump::Return(Box::new(
            materialize_expr(x, idents),
        )),
    }
}

fn materialize_use<'a>(
    spec: &UseSpec,
    idents: &'a [String],
) -> ExprUse<'a> {
    match spec {
        UseSpec::Item {
            module,
            rename,
            nested,
        } => ExprUse::Item {
            module: ident_str(idents, *module),
            rename: rename.map(|i| ident_str(idents, i)),
            nested: nested.as_ref().map(|n| {
                Box::new(materialize_use(n, idents))
            }),
        },
        UseSpec::Many(xs) => ExprUse::Many(
            xs.iter()
                .map(|x| materialize_use(x, idents))
                .collect(),
        ),
        UseSpec::Glob => ExprUse::Glob,
    }
}

#[test]
fn roundtrip_is_idempotent() {
    let seeds = [
        0x9E37_79B9_7F4A_7C15,
        0x0FED_CBA9_8765_4321,
        0xC0FF_EE00_CAFE_F00D,
        0x1234_5678_9ABC_DEF0,
    ];
    for seed in seeds {
        let mut state = seed;
        let count = 8 + pick_index(&mut state, 8);
        let idents = gen_idents(&mut state, count);
        let specs = gen_program(&mut state, idents.len());
        let ast = materialize(&specs, &idents);
        let encoded = encode(&ast);
        let decoded = decode(&encoded);
        let reencoded = encode(&decoded);
        assert_eq!(encoded, reencoded, "seed {seed:#X}");
    }
}
