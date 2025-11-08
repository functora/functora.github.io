use clap::{Args, Parser, Subcommand};
use functor_derive::Functor;
use functora_cfg::*;
use serde::{Deserialize, Serialize};
use serial_test::serial;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use temp_env::with_vars;
use tempfile::NamedTempFile;

#[derive(
    Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
)]
struct Cfg {
    host: String,
    port: u16,
    logs: bool,
    main_account: Account,
    sub_accounts: HashMap<String, Account>,
}

impl Cfg {
    pub fn new(
        cli: Cli<SubAccount>,
    ) -> Result<Self, ConfigError> {
        functora_cfg::Cfg {
            default: &Cli::def(),
            file_path: |cli| cli.toml.as_deref(),
            env_prefix: "FUNCTORA",
            command_line: &cli
                .try_fmap(|kv| kv.hash_map().map(IdClap))?,
        }
        .eval()
    }
}

#[derive(
    Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
)]
pub struct Account {
    alias: String,
    balance: i32,
    tags: Option<Vec<String>>,
}

#[derive(
    Eq,
    PartialEq,
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Subcommand,
)]
#[command(subcommand_precedence_over_arg = true)]
pub enum SubAccount {
    SubAccount(ReClap<CliAccount, Self>),
}

impl SubAccount {
    pub fn hash_map(
        self,
    ) -> Result<HashMap<String, CliAccount>, ConfigError>
    {
        let SubAccount::SubAccount(prev) = self;
        prev.hash_map(
            |k| Ok(k.to_string()),
            |SubAccount::SubAccount(next)| next,
        )
    }
}

#[derive(
    Eq,
    PartialEq,
    Debug,
    Clone,
    Functor,
    Serialize,
    Deserialize,
    Parser,
)]
#[command(version, about, long_about = None)]
struct Cli<T>
where
    T: Eq
        + PartialEq
        + Debug
        + Clone
        + Serialize
        + Subcommand,
{
    #[arg(long)]
    toml: Option<String>,
    #[arg(long)]
    host: Option<String>,
    #[arg(long)]
    port: Option<u16>,
    #[arg(long)]
    logs: Option<bool>,
    #[command(flatten)]
    main_account: Option<CliAccount>,
    #[command(subcommand)]
    sub_accounts: Option<T>,
}

#[derive(
    Eq,
    PartialEq,
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Args,
)]
pub struct CliAccount {
    #[arg(long)]
    alias: Option<String>,
    #[arg(long)]
    balance: Option<i32>,
    #[arg(long)]
    tags: Option<Vec<String>>,
}

impl Cli<IdClap<HashMap<String, CliAccount>>> {
    pub fn def() -> Self {
        Cli {
            toml: None,
            host: Some("127.0.0.1".into()),
            port: Some(8080),
            logs: Some(false),
            main_account: Some(CliAccount {
                alias: Some("Functora".into()),
                balance: Some(42),
                tags: None,
            }),
            sub_accounts: Some(IdClap(HashMap::new())),
        }
    }
}

#[test]
#[serial]
fn defaults() {
    let lhs =
        Cfg::new(Cli::parse_from(["functora"])).unwrap();
    let rhs = Cfg {
        host: "127.0.0.1".into(),
        port: 8080,
        logs: false,
        main_account: Account {
            alias: "Functora".into(),
            balance: 42,
            tags: None,
        },
        sub_accounts: HashMap::new(),
    };
    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn file_override() {
    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        host = "192.168.1.100"
        logs = true

        [sub_accounts.alice]
        alias = "Alice in Wonderland"
        balance = 101
        tags = ["retro", "story"]
    "#;
    file.write_all(text.as_bytes()).unwrap();
    let lhs = Cfg::new(Cli::parse_from([
        "functora",
        "--toml",
        &file.path().to_string_lossy(),
    ]))
    .unwrap();
    let rhs = Cfg {
        host: "192.168.1.100".into(),
        port: 8080,
        logs: true,
        main_account: Account {
            alias: "Functora".into(),
            balance: 42,
            tags: None,
        },
        sub_accounts: HashMap::from([(
            "alice".into(),
            Account {
                alias: "Alice in Wonderland".into(),
                balance: 101,
                tags: Some(vec![
                    "retro".into(),
                    "story".into(),
                ]),
            },
        )]),
    };
    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn env_override() {
    with_vars(
        vec![
            ("FUNCTORA__HOST", Some("10.0.0.1")),
            ("FUNCTORA__PORT", Some("7070")),
            ("FUNCTORA__LOGS", Some("true")),
            (
                "FUNCTORA__MAIN_ACCOUNT__ALIAS",
                Some("Rich Functora"),
            ),
            (
                "FUNCTORA__SUB_ACCOUNTS__BOB__ALIAS",
                Some("Poor Bob"),
            ),
            (
                "FUNCTORA__SUB_ACCOUNTS__BOB__BALANCE",
                Some("1"),
            ),
        ],
        || {
            let lhs =
                Cfg::new(Cli::parse_from(["functora"]))
                    .unwrap();
            let rhs = Cfg {
                host: "10.0.0.1".into(),
                port: 7070,
                logs: true,
                main_account: Account {
                    alias: "Rich Functora".into(),
                    balance: 42,
                    tags: None,
                },
                sub_accounts: HashMap::from([(
                    "bob".into(),
                    Account {
                        alias: "Poor Bob".into(),
                        balance: 1,
                        tags: None,
                    },
                )]),
            };
            assert_eq!(lhs, rhs);
        },
    );
}

#[test]
#[serial]
fn cli_override() {
    let lhs = Cfg::new(Cli::parse_from([
        "functora",
        "--port",
        "6060",
        "--logs",
        "true",
        "sub-account",
        "--alias",
        "Cli Carol",
        "--balance",
        "200",
        "--tags",
        "pure",
        "--tags",
        "geek",
    ]))
    .unwrap();
    let rhs = Cfg {
        host: "127.0.0.1".into(),
        port: 6060,
        logs: true,
        main_account: Account {
            alias: "Functora".into(),
            balance: 42,
            tags: None,
        },
        sub_accounts: HashMap::from([(
            "0".into(),
            Account {
                alias: "Cli Carol".into(),
                balance: 200,
                tags: Some(vec![
                    "pure".into(),
                    "geek".into(),
                ]),
            },
        )]),
    };
    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn layered_override() {
    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        host = "192.168.0.10"
        logs = true
        [main_account]
        alias = "File Functora"
        balance = 100
        [sub_accounts.alice]
        alias = "File Alice"
        balance = 5
    "#;
    file.write_all(text.as_bytes()).unwrap();
    with_vars(
        vec![
            ("FUNCTORA__PORT", Some("9090")),
            (
                "FUNCTORA__MAIN_ACCOUNT__BALANCE",
                Some("200"),
            ),
            (
                "FUNCTORA__SUB_ACCOUNTS__ALICE__BALANCE",
                Some("50"),
            ),
        ],
        || {
            let lhs = Cfg::new(Cli::parse_from([
                "functora",
                "--toml",
                &file.path().to_string_lossy(),
                "--host",
                "10.10.10.10",
                "--logs",
                "false",
                "sub-account",
                "--alias",
                "Cli Bob",
                "--balance",
                "999",
                "--tags",
                "vip",
                "--tags",
                "beta",
            ]))
            .unwrap();

            let rhs = Cfg {
                host: "10.10.10.10".into(),
                port: 9090,
                logs: false,
                main_account: Account {
                    alias: "File Functora".into(),
                    balance: 200,
                    tags: None,
                },
                sub_accounts: HashMap::from([
                    (
                        "alice".into(),
                        Account {
                            alias: "File Alice".into(),
                            balance: 50,
                            tags: None,
                        },
                    ),
                    (
                        "0".into(),
                        Account {
                            alias: "Cli Bob".into(),
                            balance: 999,
                            tags: Some(vec![
                                "vip".into(),
                                "beta".into(),
                            ]),
                        },
                    ),
                ]),
            };

            assert_eq!(lhs, rhs);
        },
    );
}
