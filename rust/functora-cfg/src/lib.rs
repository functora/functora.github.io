pub use config::ConfigError;
use config::{Config, Environment, File, Value};
use serde::{Serialize, de::DeserializeOwned};
use std::path::Path;
use toml::Value as TomlValue;

pub fn new<Cfg, Get, Cli>(
    app: &str,
    def: Option<&Cfg>,
    get_cfg_path: Get,
    cli: &Cli,
) -> Result<Cfg, ConfigError>
where
    Cfg: Serialize + DeserializeOwned,
    Get: Fn(&Cli) -> Option<&str>,
    Cli: Serialize,
{
    let mut builder = Config::builder();

    // Def
    if let Some(cfg) = def {
        builder = builder.add_source(term_to_config(cfg)?);
    }

    // Cfg
    if let Some(path) = get_cfg_path(cli) {
        let path = Path::new(&path);
        if path.exists() && path.is_file() {
            builder = builder.add_source(File::from(path));
        } else {
            return Err(ConfigError::Message(
                "Config file path does not exist!".into(),
            ));
        }
    }

    // Env
    builder = builder.add_source(
        Environment::with_prefix(&app.to_uppercase())
            .separator("_"),
    );

    // Cli
    builder = builder.add_source(term_to_config(cli)?);

    builder.build()?.try_deserialize()
}

fn term_to_config<T: Serialize>(
    value: &T,
) -> Result<Config, ConfigError> {
    let toml = toml::Value::try_from(value)
        .map_err(|e| ConfigError::Message(e.to_string()))?;

    if let TomlValue::Table(kv) = toml {
        kv.into_iter()
            .try_fold(Config::builder(), |acc, (k, v)| {
                acc.set_override(k, toml_to_config(v))
            })?
            .build()
    } else {
        Err(ConfigError::Message(
            "Expected table at root".into(),
        ))
    }
}

fn toml_to_config(toml: TomlValue) -> Value {
    match toml {
        TomlValue::String(x) => Value::from(x),
        TomlValue::Integer(x) => Value::from(x),
        TomlValue::Float(x) => Value::from(x),
        TomlValue::Boolean(x) => Value::from(x),
        TomlValue::Datetime(x) => {
            Value::from(x.to_string())
        }
        TomlValue::Array(xs) => Value::from(
            xs.into_iter()
                .map(toml_to_config)
                .collect::<Vec<_>>(),
        ),
        TomlValue::Table(kv) => Value::from(
            kv.into_iter()
                .map(|(k, v)| (k, toml_to_config(v)))
                .collect::<config::Map<String, Value>>(),
        ),
    }
}
