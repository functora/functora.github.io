use serde::{Deserialize, Serialize};
use serial_test::serial;
use temp_env::with_vars;

fn new_cfg(cli: &Cli) -> Cfg {
    let def = Cli {
        cfg: None,
        host: Some("127.0.0.1".into()),
        port: Some(8080),
        debug: Some(false),
        nested: Some(CfgNest {
            name: "foo".into(),
            value: 42,
        }),
        tags: None,
    };
    functora_cfg::Args {
        default: &def,
        file_path: |cli: &Cli| cli.cfg.as_deref(),
        env_prefix: "fun_app",
        command_line: cli,
    }
    .eval()
    .unwrap()
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Cfg {
    host: String,
    port: u16,
    debug: bool,
    nested: Option<CfgNest>,
    tags: Option<Vec<String>>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct CfgNest {
    name: String,
    value: i32,
}

#[derive(Debug, Serialize)]
struct Cli {
    cfg: Option<String>,
    host: Option<String>,
    port: Option<u16>,
    debug: Option<bool>,
    nested: Option<CfgNest>,
    tags: Option<Vec<String>>,
}

#[test]
#[serial]
fn defaults_only() {
    let def = Cfg {
        host: "127.0.0.1".into(),
        port: 8080,
        debug: false,
        nested: Some(CfgNest {
            name: "foo".into(),
            value: 42,
        }),
        tags: None,
    };
    let cli = Cli {
        cfg: None,
        host: None,
        port: None,
        debug: None,
        nested: None,
        tags: None,
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg, def);
}

#[test]
#[serial]
fn with_file_override() {
    let path = std::env::temp_dir().join("fun.toml");
    let file = r#"
        host = "192.168.1.100"
        port = 9090
        debug = true
        tags = ["a", "b"]
    "#;
    std::fs::write(&path, file).unwrap();
    let cli = Cli {
        cfg: Some(path.to_string_lossy().into_owned()),
        host: None,
        port: None,
        debug: None,
        nested: None,
        tags: None,
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg.host, "192.168.1.100");
    assert_eq!(cfg.port, 9090);
    assert_eq!(cfg.debug, true);
    assert_eq!(cfg.tags.unwrap(), vec!["a", "b"]);
}

#[test]
#[serial]
fn env_override() {
    let cli = Cli {
        cfg: None,
        host: None,
        port: None,
        debug: None,
        nested: None,
        tags: None,
    };
    with_vars(
        vec![
            ("FUN_APP_HOST", Some("10.0.0.1")),
            ("FUN_APP_PORT", Some("7070")),
            ("FUN_APP_DEBUG", Some("true")),
        ],
        || {
            let cfg: Cfg = new_cfg(&cli);
            assert_eq!(cfg.host, "10.0.0.1");
            assert_eq!(cfg.port, 7070);
            assert_eq!(cfg.debug, true);
        },
    );
}

#[test]
#[serial]
fn cli_override() {
    let cli = Cli {
        cfg: None,
        host: Some("cli.host".into()),
        port: Some(6060),
        debug: Some(true),
        nested: None,
        tags: Some(vec!["cli1".into(), "cli2".into()]),
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg.host, "cli.host");
    assert_eq!(cfg.port, 6060);
    assert_eq!(cfg.debug, true);
    assert_eq!(cfg.tags.unwrap(), vec!["cli1", "cli2"]);
}

#[test]
#[serial]
fn nested_struct() {
    let cli = Cli {
        cfg: None,
        host: None,
        port: None,
        debug: None,
        nested: None,
        tags: None,
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg.nested.as_ref().unwrap().name, "foo");
    assert_eq!(cfg.nested.as_ref().unwrap().value, 42);
}
