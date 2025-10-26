# functora-cfg

A Rust library that merges configuration values from multiple sources into a single typed value. Configuration values are applied in the following order:

- Defaults
- Config file
- Environment variables
- Command-line arguments

All sources are optional. Only the ones you provide will be applied.

## Example

```text
use clap::{Parser, Subcommand};
use functora_cfg;
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Clone, Serialize, Deserialize, PartialEq,
)]
pub struct Cfg {
    pub conn: String,
    pub logs: String,
    pub many: Vec<String>,
    pub nest: CfgNest,
}

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    PartialEq,
    Subcommand,
)]
pub enum CfgNest {
    Nest { name: String, value: i32 },
}

#[derive(Debug, Clone, Serialize, Parser)]
#[command(version, about)]
pub struct Cli {
    #[arg(long)]
    pub toml: Option<String>,
    #[arg(long)]
    pub conn: Option<String>,
    #[arg(long)]
    pub logs: Option<String>,
    #[arg(long)]
    pub many: Option<Vec<String>>,
    #[command(subcommand)]
    pub nest: Option<CfgNest>,
}

fn new_cfg(cli: &Cli) -> Cfg {
    let defaults = Cli {
        toml: None,
        conn: Some("postgres://localhost".into()),
        logs: Some("/var/log/app.log".into()),
        many: Some(vec!["a".into(), "b".into()]),
        nest: Some(CfgNest::Nest {
            name: "foo".into(),
            value: 42,
        }),
    };

    functora_cfg::Cfg {
        default: &defaults,
        file_path: |cli: &Cli| cli.toml.as_deref(),
        env_prefix: "FUNCTORA",
        command_line: cli,
    }
    .eval()
    .unwrap()
}

fn main() {
    let cfg = new_cfg(&Cli::parse());
    println!("{:#?}", cfg);
}
```

Run with defaults:

```shell
cargo run
```

Use a config file to override defaults:

```shell
cargo run -- --toml ./functora.toml
```

Example `functora.toml`:

```toml
conn = "postgres://remote"
logs = "/tmp/app.log"
many = ["x", "y", "z"]

[nest.Nest]
name = "file_nested"
value = 99
```

Environment variables override defaults and file values:

```shell
FUNCTORA_CONN="./sqlite.db" cargo run -- --toml ./functora.toml
```

Command-line arguments override all other sources:

```shell
cargo run -- --toml ./functora.toml --conn "./functora.db"
```

<hr>

© 2025 [Functora](https://functora.github.io/). All rights reserved.
