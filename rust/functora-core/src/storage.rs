use crate::error::{Error, WorkerStopped};
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::Value;
use serde_json::from_str;
use serde_json::from_value;
use serde_json::to_string_pretty;
use serde_json::to_value;
use std::fs::{OpenOptions, read_to_string, write};
use std::path::Path;
use std::sync::Mutex;

/// Serializes read-modify-write cycles on the storage file so concurrent persist
/// tasks cannot lose each other's key updates or read a torn half-written file.
static STORAGE_LOCK: Mutex<()> = Mutex::new(());

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
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    let p = path.as_ref();
    ensure_file(p)?;
    let content = read_to_string(p)?;
    let mut json: Value = from_str(&content)?;
    if let Some(obj) = json.as_object_mut() {
        _ = obj.insert(key.to_string(), to_value(val)?);
        let s = to_string_pretty(&json)?;
        Ok(write(p, s)?)
    } else {
        Err(Error::NotJsonObject(json))
    }
}

pub fn find_or_init_key<
    P: AsRef<Path>,
    T: DeserializeOwned + Clone + Serialize,
    F: FnOnce() -> T,
>(
    path: P,
    key: &str,
    init: F,
) -> Result<T, Error> {
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    let p = path.as_ref();
    ensure_file(p)?;
    let content = read_to_string(p)?;
    let json: Value = from_str(&content)?;
    if let Some(val) = json.get(key) {
        Ok(from_value(val.clone())?)
    } else {
        let val = init();
        let mut json_mut = json;
        if let Some(obj) = json_mut.as_object_mut() {
            _ = obj.insert(key.to_string(), to_value(&val)?);
            let s = to_string_pretty(&json_mut)?;
            write(p, s)?;
            Ok(val)
        } else {
            Err(Error::NotJsonObject(json_mut))
        }
    }
}

pub fn read_json_object<P: AsRef<Path>>(path: P) -> Result<Value, Error> {
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    Ok(from_str(&read_to_string(path.as_ref())?)?)
}

pub fn write_json_object<P: AsRef<Path>>(path: P, json: &Value) -> Result<(), Error> {
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    let s = to_string_pretty(&json)?;
    Ok(write(path.as_ref(), s)?)
}

pub fn get_json_value<P: AsRef<Path>>(path: P, key: &str) -> Result<Option<Value>, Error> {
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    let json: Value = from_str(&read_to_string(path.as_ref())?)?;
    match json.as_object() {
        Some(obj) => Ok(obj.get(key).cloned()),
        None => Err(Error::NotJsonObject(json)),
    }
}

pub fn set_json_value<P: AsRef<Path>, T: Serialize>(
    path: P,
    key: &str,
    val: T,
) -> Result<(), Error> {
    let _guard = STORAGE_LOCK
        .lock()
        .map_err(|_| Error::Worker(WorkerStopped))?;
    let p = path.as_ref();
    ensure_file(p)?;
    let mut json: Value = from_str(&read_to_string(p)?)?;
    let value = to_value(val)?;
    if let Some(obj) = json.as_object_mut() {
        _ = obj.insert(key.to_string(), value);
        let s = to_string_pretty(&json)?;
        Ok(write(p, s)?)
    } else {
        Err(Error::NotJsonObject(json))
    }
}
