pub use config::ConfigError;
use config::{Config, Environment, File, Value};
use serde::{Serialize, de::DeserializeOwned};
use std::path::Path;
use toml::Value as TomlValue;

pub struct Args<'a, Src: Serialize> {
    pub default: &'a Src,
    pub file_path: fn(&Src) -> Option<&str>,
    pub env_prefix: &'a str,
    pub command_line: &'a Src,
}

impl<'a, Src: Serialize> Args<'a, Src> {
    pub fn eval<Cfg: Serialize + DeserializeOwned>(
        &self,
    ) -> Result<Cfg, ConfigError> {
        new(self)
    }
}

pub fn new<Src, Cfg>(
    args: &Args<Src>,
) -> Result<Cfg, ConfigError>
where
    Src: Serialize,
    Cfg: Serialize + DeserializeOwned,
{
    //
    // TODO : use functional style with tierator over
    // trait object array and try_fold into builder,
    // maybe somthing else, this looks too bad:
    //

    let mut builder = Config::builder();

    builder =
        builder.add_source(term_to_config(args.default)?);

    let getter = args.file_path;
    if let Some(path) =
        getter(args.command_line).or(getter(args.default))
    {
        let path = Path::new(&path);
        if path.exists() && path.is_file() {
            builder = builder.add_source(File::from(path));
        } else {
            return Err(ConfigError::Message(
                "Config file path does not exist!".into(),
            ));
        }
    }

    builder = builder.add_source(
        Environment::with_prefix(
            &args.env_prefix.to_uppercase(),
        )
        .separator("_"),
    );

    builder = builder
        .add_source(term_to_config(args.command_line)?);

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
