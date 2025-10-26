use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serial_test::serial;
use temp_env::with_vars;

fn new_cfg(cli: &Cli) -> Cfg {
    let def = Cli {
        cfg: None,
        host: Some("127.0.0.1".into()),
        port: Some(8080),
        debug: Some(false),
        nest_val: Some(CfgNest {
            name: "foo".into(),
            value: 42,
        }),
        many_val: HashMap::new(),
        tags: None,
    };
    functora_cfg::Cfg {
        default: &def,
        file_path: |cli: &Cli| cli.cfg.as_deref(),
        env_prefix: "FUNCTORA",
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
    nest_val: Option<CfgNest>,
    many_val: HashMap<usize, CfgNest>,
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
    nest_val: Option<CfgNest>,
    many_val: HashMap<usize, CfgNest>,
    tags: Option<Vec<String>>,
}

#[test]
#[serial]
fn defaults_only() {
    let def = Cfg {
        host: "127.0.0.1".into(),
        port: 8080,
        debug: false,
        nest_val: Some(CfgNest {
            name: "foo".into(),
            value: 42,
        }),
        many_val: HashMap::new(),
        tags: None,
    };
    let cli = Cli {
        cfg: None,
        host: None,
        port: None,
        debug: None,
        nest_val: None,
        many_val: HashMap::new(),
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

        [many_val.1]
        name = "hello"
        value = 123
    "#;
    std::fs::write(&path, file).unwrap();
    let cli = Cli {
        cfg: Some(path.to_string_lossy().into_owned()),
        host: None,
        port: None,
        debug: None,
        nest_val: None,
        many_val: HashMap::new(),
        tags: None,
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg.host, "192.168.1.100");
    assert_eq!(cfg.port, 9090);
    assert_eq!(cfg.debug, true);
    assert_eq!(cfg.tags.unwrap(), vec!["a", "b"]);
    assert_eq!(
        cfg.many_val,
        HashMap::from([(
            1,
            CfgNest {
                name: "hello".into(),
                value: 123
            }
        )])
    );
}

#[test]
#[serial]
fn env_override() {
    let cli = Cli {
        cfg: None,
        host: None,
        port: None,
        debug: None,
        nest_val: None,
        many_val: HashMap::new(),
        tags: None,
    };
    with_vars(
        vec![
            ("FUNCTORA__HOST", Some("10.0.0.1")),
            ("FUNCTORA__PORT", Some("7070")),
            ("FUNCTORA__DEBUG", Some("true")),
            ("FUNCTORA__NEST_VAL__NAME", Some("bar")),
            ("FUNCTORA__MANY_VAL__0__NAME", Some("buz")),
            ("FUNCTORA__MANY_VAL__0__VALUE", Some("1")),
        ],
        || {
            let cfg: Cfg = new_cfg(&cli);
            assert_eq!(cfg.host, "10.0.0.1");
            assert_eq!(cfg.port, 7070);
            assert_eq!(cfg.debug, true);
            assert_eq!(
                cfg.nest_val,
                Some(CfgNest {
                    name: "bar".into(),
                    value: 42
                })
            );
            assert_eq!(
                cfg.many_val,
                HashMap::from([(
                    0,
                    CfgNest {
                        name: "buz".into(),
                        value: 1
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
        cfg: None,
        host: Some("cli.host".into()),
        port: Some(6060),
        debug: Some(true),
        nest_val: None,
        many_val: HashMap::new(),
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
        nest_val: None,
        many_val: HashMap::new(),
        tags: None,
    };
    let cfg: Cfg = new_cfg(&cli);
    assert_eq!(cfg.nest_val.as_ref().unwrap().name, "foo");
    assert_eq!(cfg.nest_val.as_ref().unwrap().value, 42);
}
