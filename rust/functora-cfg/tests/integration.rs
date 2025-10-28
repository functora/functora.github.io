use clap::{Args, Parser, Subcommand};
use functor_derive::Functor;
use functora_cfg::*;
use serde::{Deserialize, Serialize};
use serial_test::serial;
use std::collections::HashMap;
use std::fmt::Debug;
use temp_env::with_vars;

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
    pub fn new(cli: Cli<SubAccounts>) -> Self {
        functora_cfg::Cfg {
            default: &Cli::def(),
            file_path: |cli| cli.toml.as_deref(),
            env_prefix: "FUNCTORA",
            command_line: &cli
                .fmap(|x| IdClap(x.hash_map())),
        }
        .eval()
        .unwrap()
    }
}

#[derive(
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Args,
)]
pub struct Account {
    #[arg(long)]
    alias: String,
    #[arg(long)]
    balance: i32,
    #[arg(long)]
    tags: Option<Vec<String>>,
}

#[derive(
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Subcommand,
)]
#[command(subcommand_precedence_over_arg = true)]
pub enum SubAccounts {
    SubAccounts(ReClap<Account, Self>),
}

impl SubAccounts {
    pub fn vec(self) -> Vec<Account> {
        let SubAccounts::SubAccounts(prev) = self;
        prev.vec(|SubAccounts::SubAccounts(next)| next)
    }

    pub fn hash_map(self) -> HashMap<String, Account> {
        let SubAccounts::SubAccounts(prev) = self;
        prev.hash_map(|SubAccounts::SubAccounts(next)| next)
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
    main_account: Option<Account>,
    #[command(subcommand)]
    sub_accounts: Option<T>,
}

impl Cli<IdClap<HashMap<String, Account>>> {
    pub fn def() -> Self {
        Cli {
            toml: None,
            host: Some("127.0.0.1".into()),
            port: Some(8080),
            logs: Some(false),
            main_account: Some(Account {
                alias: "foo".into(),
                balance: 42,
                tags: None,
            }),
            sub_accounts: Some(IdClap(HashMap::new())),
        }
    }
}

#[test]
#[serial]
fn defaults_only() {
    let cli = Cli::parse_from(["test"]);
    let lhs = Cfg::new(cli);
    // let lhs = Cfg::new(Cli {
    //     toml: None,
    //     host: None,
    //     port: None,
    //     logs: None,
    //     main_account: None,
    //     sub_accounts: SubAccounts::SubAccounts(ReClap {
    //         prev: None,
    //         next: None,
    //     }),
    // });
    let rhs = Cfg {
        host: "127.0.0.1".into(),
        port: 8080,
        logs: false,
        main_account: Account {
            alias: "foo".into(),
            balance: 42,
            tags: None,
        },
        sub_accounts: HashMap::new(),
    };
    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn with_file_override() {
    let path = std::env::temp_dir().join("fun.toml");
    let file = r#"
        host = "192.168.1.100"
        port = 9090
        logs = true
        tags = ["a", "b"]

        [sub_accounts.alice]
        alias = "hello"
        balance = 123
    "#;
    std::fs::write(&path, file).unwrap();
    let cli = Cli {
        toml: Some(path.to_string_lossy().into_owned()),
        host: None,
        port: None,
        logs: None,
        main_account: None,
        sub_accounts: None,
    };
    let cfg: Cfg = Cfg::new(cli);
    assert_eq!(cfg.host, "192.168.1.100");
    assert_eq!(cfg.port, 9090);
    assert_eq!(cfg.logs, true);
    assert_eq!(
        cfg.sub_accounts,
        HashMap::from([(
            "alice".into(),
            Account {
                alias: "hello".into(),
                balance: 123,
                tags: None,
            }
        )])
    );
}

#[test]
#[serial]
fn env_override() {
    let cli = Cli {
        toml: None,
        host: None,
        port: None,
        logs: None,
        main_account: None,
        sub_accounts: None,
    };
    with_vars(
        vec![
            ("FUNCTORA__HOST", Some("10.0.0.1")),
            ("FUNCTORA__PORT", Some("7070")),
            ("FUNCTORA__LOGS", Some("true")),
            ("FUNCTORA__MAIN_ACCOUNT__ALIAS", Some("bar")),
            (
                "FUNCTORA__SUB_ACCOUNTS__BOB__ALIAS",
                Some("buz"),
            ),
            (
                "FUNCTORA__SUB_ACCOUNTS__BOB__BALANCE",
                Some("1"),
            ),
        ],
        || {
            let cfg: Cfg = Cfg::new(cli);
            assert_eq!(cfg.host, "10.0.0.1");
            assert_eq!(cfg.port, 7070);
            assert_eq!(cfg.logs, true);
            assert_eq!(
                cfg.main_account,
                Account {
                    alias: "bar".into(),
                    balance: 42,
                    tags: None,
                }
            );
            assert_eq!(
                cfg.sub_accounts,
                HashMap::from([(
                    "bob".into(),
                    Account {
                        alias: "buz".into(),
                        balance: 1,
                        tags: None,
                    }
                )])
            );
        },
    );
}

#[test]
#[serial]
fn cli_override() {
    let cli = Cli {
        toml: None,
        host: Some("cli.host".into()),
        port: Some(6060),
        logs: Some(true),
        main_account: None,
        sub_accounts: None,
    };
    let cfg: Cfg = Cfg::new(cli);
    assert_eq!(cfg.host, "cli.host");
    assert_eq!(cfg.port, 6060);
    assert_eq!(cfg.logs, true);
}

#[test]
#[serial]
fn nested_struct() {
    let cli = Cli {
        toml: None,
        host: None,
        port: None,
        logs: None,
        main_account: None,
        sub_accounts: None,
    };
    let cfg: Cfg = Cfg::new(cli);
    assert_eq!(cfg.main_account.alias, "foo");
    assert_eq!(cfg.main_account.balance, 42);
}
