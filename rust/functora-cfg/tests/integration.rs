#![allow(unused_results)]
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
            file_path: |c| c.toml.as_deref(),
            env_prefix: "FUNCTORA",
            command_line: &cli
                .try_fmap(|kv| kv.hash_map().map(IdClap))?,
            transform_ast: substitute_defaults,
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

#[derive(
    Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
)]
struct DatabaseCfg {
    connections: HashMap<String, Connection>,
}

#[derive(
    Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
)]
pub struct Connection {
    host: String,
    port: u16,
    timeout: u32,
    ssl: bool,
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
struct DbCli<T>
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
    #[command(subcommand)]
    connections: Option<T>,
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
pub enum SubConnection {
    SubConnection(ReClap<DbCliConnection, Self>),
}

impl SubConnection {
    pub fn hash_map(
        self,
    ) -> Result<HashMap<String, DbCliConnection>, ConfigError>
    {
        let SubConnection::SubConnection(prev) = self;
        prev.hash_map(
            |k| Ok(k.to_string()),
            |SubConnection::SubConnection(next)| next,
        )
    }
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
pub struct DbCliConnection {
    #[arg(long)]
    name: Option<String>,
    #[arg(long)]
    host: Option<String>,
    #[arg(long)]
    port: Option<u16>,
    #[arg(long)]
    timeout: Option<u32>,
    #[arg(long)]
    ssl: Option<bool>,
}

impl DbCli<IdClap<HashMap<String, DbCliConnection>>> {
    pub fn def() -> Self {
        DbCli {
            toml: None,
            connections: Some(IdClap(HashMap::new())),
        }
    }
}

impl DbCli<SubConnection> {
    pub fn new_with_defaults(
        cli: DbCli<SubConnection>,
    ) -> Result<DatabaseCfg, ConfigError> {
        functora_cfg::Cfg {
            default: &DbCli::def(),
            file_path: |c| c.toml.as_deref(),
            env_prefix: "DATABASE",
            command_line: &cli.try_fmap(|conn| {
                conn.hash_map().map(IdClap)
            })?,
            transform_ast: substitute_defaults,
        }
        .eval()
    }
}

#[test]
#[serial]
fn substitute_defaults_from_file() {
    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        [connections]
        [connections.default]
        host = "localhost"
        port = 5432
        timeout = 30
        ssl = true

        [connections.primary]
        name = "Primary DB"

        [connections.replica]
        name = "Replica DB"
        port = 5433
    "#;
    file.write_all(text.as_bytes()).unwrap();

    let lhs = DbCli::<SubConnection>::new_with_defaults(
        DbCli::parse_from([
            "dbcli",
            "--toml",
            &file.path().to_string_lossy(),
        ]),
    )
    .unwrap();

    let rhs = DatabaseCfg {
        connections: HashMap::from([
            (
                "primary".into(),
                Connection {
                    host: "localhost".into(),
                    port: 5432,
                    timeout: 30,
                    ssl: true,
                },
            ),
            (
                "replica".into(),
                Connection {
                    host: "localhost".into(),
                    port: 5433,
                    timeout: 30,
                    ssl: true,
                },
            ),
        ]),
    };

    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn substitute_defaults_partial_override() {
    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        [connections]
        [connections.default]
        host = "db.example.com"
        port = 3306
        timeout = 60
        ssl = false

        [connections.analytics]
        name = "Analytics"
        host = "analytics.internal"
        timeout = 120

        [connections.transactions]
        name = "Transactions"
    "#;
    file.write_all(text.as_bytes()).unwrap();

    let lhs = DbCli::<SubConnection>::new_with_defaults(
        DbCli::parse_from([
            "dbcli",
            "--toml",
            &file.path().to_string_lossy(),
        ]),
    )
    .unwrap();

    let rhs = DatabaseCfg {
        connections: HashMap::from([
            (
                "analytics".into(),
                Connection {
                    host: "analytics.internal".into(),
                    port: 3306,
                    timeout: 120,
                    ssl: false,
                },
            ),
            (
                "transactions".into(),
                Connection {
                    host: "db.example.com".into(),
                    port: 3306,
                    timeout: 60,
                    ssl: false,
                },
            ),
        ]),
    };

    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn substitute_defaults_with_cli_override() {
    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        [connections]
        [connections.default]
        host = "localhost"
        port = 5432
        timeout = 30
        ssl = true

        [connections.cache]
        name = "Cache DB"
    "#;
    file.write_all(text.as_bytes()).unwrap();

    let lhs = DbCli::<SubConnection>::new_with_defaults(
        DbCli::parse_from([
            "dbcli",
            "--toml",
            &file.path().to_string_lossy(),
            "sub-connection",
            "--name",
            "Session Store",
            "--host",
            "redis.local",
            "--port",
            "6379",
        ]),
    )
    .unwrap();

    let rhs = DatabaseCfg {
        connections: HashMap::from([
            (
                "cache".into(),
                Connection {
                    host: "localhost".into(),
                    port: 5432,
                    timeout: 30,
                    ssl: true,
                },
            ),
            (
                "0".into(),
                Connection {
                    host: "redis.local".into(),
                    port: 6379,
                    timeout: 30,
                    ssl: true,
                },
            ),
        ]),
    };

    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn substitute_defaults_nested_tables() {
    #[derive(
        Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
    )]
    struct ServiceCfg {
        services: HashMap<String, Service>,
    }

    #[derive(
        Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
    )]
    struct Service {
        image: String,
        replicas: u32,
        cpu: String,
        memory: String,
    }

    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        [services]
        [services.default]
        image = "myapp:latest"
        replicas = 1
        cpu = "100m"
        memory = "128Mi"

        [services.api]
        replicas = 3
        cpu = "500m"

        [services.worker]
        image = "worker:v2"
    "#;
    file.write_all(text.as_bytes()).unwrap();

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
    struct ServiceCli<T>
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
        #[command(subcommand)]
        services: Option<T>,
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
    pub enum SubService {
        SubService(ReClap<ServiceCliService, Self>),
    }

    impl SubService {
        pub fn hash_map(
            self,
        ) -> Result<
            HashMap<String, ServiceCliService>,
            ConfigError,
        > {
            let SubService::SubService(prev) = self;
            prev.hash_map(
                |k| Ok(k.to_string()),
                |SubService::SubService(next)| next,
            )
        }
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
    pub struct ServiceCliService {
        #[arg(long)]
        name: Option<String>,
        #[arg(long)]
        image: Option<String>,
        #[arg(long)]
        replicas: Option<u32>,
        #[arg(long)]
        cpu: Option<String>,
        #[arg(long)]
        memory: Option<String>,
    }

    impl
        ServiceCli<
            IdClap<HashMap<String, ServiceCliService>>,
        >
    {
        pub fn def() -> Self {
            ServiceCli {
                toml: None,
                services: Some(IdClap(HashMap::new())),
            }
        }
    }

    impl ServiceCli<SubService> {
        pub fn new_with_defaults(
            cli: ServiceCli<SubService>,
        ) -> Result<ServiceCfg, ConfigError> {
            functora_cfg::Cfg {
                default: &ServiceCli::def(),
                file_path: |c| c.toml.as_deref(),
                env_prefix: "SERVICE",
                command_line: &cli.try_fmap(|svc| {
                    svc.hash_map().map(IdClap)
                })?,
                transform_ast: substitute_defaults,
            }
            .eval()
        }
    }

    let lhs = ServiceCli::<SubService>::new_with_defaults(
        ServiceCli::parse_from([
            "servicecli",
            "--toml",
            &file.path().to_string_lossy(),
        ]),
    )
    .unwrap();

    let rhs = ServiceCfg {
        services: HashMap::from([
            (
                "api".into(),
                Service {
                    image: "myapp:latest".into(),
                    replicas: 3,
                    cpu: "500m".into(),
                    memory: "128Mi".into(),
                },
            ),
            (
                "worker".into(),
                Service {
                    image: "worker:v2".into(),
                    replicas: 1,
                    cpu: "100m".into(),
                    memory: "128Mi".into(),
                },
            ),
        ]),
    };

    assert_eq!(lhs, rhs);
}

#[test]
#[serial]
fn substitute_defaults_array_values() {
    #[derive(
        Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
    )]
    struct TeamCfg {
        teams: HashMap<String, Team>,
    }

    #[derive(
        Eq, PartialEq, Debug, Clone, Serialize, Deserialize,
    )]
    struct Team {
        name: String,
        members: Vec<String>,
        permissions: Vec<String>,
    }

    let mut file =
        NamedTempFile::with_suffix(".toml").unwrap();
    let text = r#"
        [teams]
        [teams.default]
        members = ["guest"]
        permissions = ["read"]

        [teams.admins]
        name = "Administrators"
        members = ["alice", "bob"]
        permissions = ["read", "write", "delete"]

        [teams.developers]
        name = "Developers"
        members = ["carol", "dave"]
    "#;
    file.write_all(text.as_bytes()).unwrap();

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
    struct TeamCli<T>
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
        #[command(subcommand)]
        teams: Option<T>,
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
    pub enum SubTeam {
        SubTeam(ReClap<TeamCliTeam, Self>),
    }

    impl SubTeam {
        pub fn hash_map(
            self,
        ) -> Result<HashMap<String, TeamCliTeam>, ConfigError>
        {
            let SubTeam::SubTeam(prev) = self;
            prev.hash_map(
                |k| Ok(k.to_string()),
                |SubTeam::SubTeam(next)| next,
            )
        }
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
    pub struct TeamCliTeam {
        #[arg(long)]
        name: Option<String>,
        #[arg(long)]
        members: Option<Vec<String>>,
        #[arg(long)]
        permissions: Option<Vec<String>>,
    }

    impl TeamCli<IdClap<HashMap<String, TeamCliTeam>>> {
        pub fn def() -> Self {
            TeamCli {
                toml: None,
                teams: Some(IdClap(HashMap::new())),
            }
        }
    }

    impl TeamCli<SubTeam> {
        pub fn new_with_defaults(
            cli: TeamCli<SubTeam>,
        ) -> Result<TeamCfg, ConfigError> {
            functora_cfg::Cfg {
                default: &TeamCli::def(),
                file_path: |c| c.toml.as_deref(),
                env_prefix: "TEAM",
                command_line: &cli.try_fmap(|team| {
                    team.hash_map().map(IdClap)
                })?,
                transform_ast: substitute_defaults,
            }
            .eval()
        }
    }

    let lhs = TeamCli::<SubTeam>::new_with_defaults(
        TeamCli::parse_from([
            "teamcli",
            "--toml",
            &file.path().to_string_lossy(),
        ]),
    )
    .unwrap();

    let rhs = TeamCfg {
        teams: HashMap::from([
            (
                "admins".into(),
                Team {
                    name: "Administrators".into(),
                    members: vec![
                        "alice".into(),
                        "bob".into(),
                    ],
                    permissions: vec![
                        "read".into(),
                        "write".into(),
                        "delete".into(),
                    ],
                },
            ),
            (
                "developers".into(),
                Team {
                    name: "Developers".into(),
                    members: vec![
                        "carol".into(),
                        "dave".into(),
                    ],
                    permissions: vec!["read".into()],
                },
            ),
        ]),
    };

    assert_eq!(lhs, rhs);
}
