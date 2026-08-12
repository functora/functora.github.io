use crate::error::Error;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::{Value, from_str, from_value, to_string_pretty, to_value};
use std::fs::{OpenOptions, read_to_string, write};
use std::path::Path;

#[cfg(target_os = "android")]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    crate::android::get_files_dir()
}

#[cfg(target_os = "ios")]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    Ok(std::env::var("HOME").map(|path| std::path::PathBuf::from(path).join("Documents"))?)
}

#[cfg(not(any(target_os = "android", target_os = "ios")))]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    Ok(std::env::current_dir()?)
}

fn ensure_file(p: &Path) -> Result<(), Error> {
    let empty = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(p)?
        .metadata()?
        .len()
        == 0;
    if empty { Ok(write(p, b"{}")?) } else { Ok(()) }
}

pub fn update_key<P: AsRef<Path>, T: Serialize>(path: P, key: &str, val: T) -> Result<(), Error> {
    let p = path.as_ref();
    ensure_file(p)?;
    let content = read_to_string(p)?;
    let mut json: Value = from_str(&content)?;
    let Some(obj) = json.as_object_mut() else {
        return Err(Error::NotJsonObject(json));
    };
    _ = obj.insert(key.to_string(), to_value(val)?);
    let s = to_string_pretty(&json)?;
    Ok(write(p, s)?)
}

pub fn find_or_init_key<P: AsRef<Path>, T: DeserializeOwned + Clone + Serialize, F: FnOnce() -> T>(
    path: P,
    key: &str,
    init: F,
) -> Result<T, Error> {
    let p = path.as_ref();
    let content = read_to_string(p)?;
    let json: Value = from_str(&content)?;
    if let Some(val) = json.get(key) {
        Ok(from_value(val.clone())?)
    } else {
        let val = init();
        update_key(p, key, &val)?;
        Ok(val)
    }
}

pub fn use_storage<T: Serialize + DeserializeOwned + Clone + PartialEq + 'static>(
    key: &'static str,
    init: impl FnOnce() -> T,
) -> Result<dioxus::prelude::Signal<T>, Error> {
    let path = files_dir()?.join("storage.json");
    ensure_file(&path)?;
    Ok(dioxus::prelude::Signal::new(find_or_init_key(&path, key, init)?))
}

pub fn read_json_object<P: AsRef<Path>>(path: P) -> Result<Value, Error> {
    Ok(from_str(&read_to_string(path)?)?)
}

pub fn write_json_object<P: AsRef<Path>>(path: P, json: &Value) -> Result<(), Error> {
    let s = to_string_pretty(&json)?;
    Ok(write(path, s)?)
}

pub fn get_json_value<P: AsRef<Path>>(path: P, key: &str) -> Result<Option<Value>, Error> {
    let json = read_json_object(path)?;
    match json.as_object() {
        Some(obj) => Ok(obj.get(key).cloned()),
        None => Err(Error::NotJsonObject(json)),
    }
}

pub fn set_json_value<P: AsRef<Path>, T: Serialize>(path: P, key: &str, val: T) -> Result<(), Error> {
    let mut json = read_json_object(&path)?;
    let value = to_value(val)?;
    let Some(obj) = json.as_object_mut() else {
        return Err(Error::NotJsonObject(json));
    };
    _ = obj.insert(key.to_string(), value);
    write_json_object(path, &json)
}
