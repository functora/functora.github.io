use crate::error::Error;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::{Value, from_str, from_value, to_string_pretty, to_value};
use std::fs::{OpenOptions, read_to_string, write};
use std::path::Path;
use tap::Pipe;

const NOT_JSON_OBJECT: &str = "Not a JSON object";

#[cfg(target_os = "android")]
fn jni_dispatch<T: Send + 'static>(
    f: impl FnOnce(&mut jni::JNIEnv, &jni::objects::JObject) -> Result<T, jni::errors::Error> + Send + 'static,
) -> Result<T, Error> {
    use std::sync::mpsc::channel;
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut jni::JNIEnv, activity: &jni::objects::JObject, _| {
        _ = tx.send(f(env, activity).map_err(Error::from));
    });
    rx.recv()?
}

#[cfg(target_os = "android")]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    use jni::objects::JString;
    jni_dispatch(|env, activity| {
        env.call_method(activity, "getFilesDir", "()Ljava/io/File;", &[])
            .and_then(|v| v.l())
            .and_then(|f| env.call_method(f, "getAbsolutePath", "()Ljava/lang/String;", &[]))
            .and_then(|v| v.l())
            .map(JString::from)
            .and_then(|s| env.get_string(&s).map(String::from))
            .map(std::path::PathBuf::from)
    })
}

#[cfg(target_os = "ios")]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    std::env::var("HOME")
        .map(|path| std::path::PathBuf::from(path).join("Documents"))
        .map_err(Error::from)
}

#[cfg(not(any(target_os = "android", target_os = "ios")))]
pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    std::env::current_dir().map_err(Error::from)
}

fn ensure_file(p: &Path) -> Result<(), Error> {
    let empty = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(p)
        .map_err(Error::from)?
        .metadata()
        .map_err(Error::from)?
        .len()
        == 0;
    if empty {
        write(p, b"{}").map_err(Error::from)
    } else {
        Ok(())
    }
}

pub fn update_key<P: AsRef<Path>, T: Serialize>(path: P, key: &str, val: T) -> Result<(), Error> {
    let p = path.as_ref();
    ensure_file(p)?;
    let content = read_to_string(p)?;
    let mut json: Value = from_str(&content)?;
    let obj = json
        .as_object_mut()
        .ok_or_else(|| Error::NotJsonObject(NOT_JSON_OBJECT.to_string()))?;
    _ = obj.insert(key.to_string(), to_value(val)?);
    let s = to_string_pretty(&json)?;
    write(p, s).map_err(Error::from)
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
        from_value(val.clone()).map_err(Error::from)
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
    from_str(&read_to_string(path)?).map_err(Error::from)
}

pub fn write_json_object<P: AsRef<Path>>(path: P, json: &Value) -> Result<(), Error> {
    to_string_pretty(&json)
        .map_err(Error::from)
        .and_then(|s| write(path, s).map_err(Error::from))
}

pub fn get_json_value<P: AsRef<Path>>(path: P, key: &str) -> Result<Option<Value>, Error> {
    let json = read_json_object(path)?;
    json.as_object()
        .ok_or_else(|| Error::NotJsonObject(NOT_JSON_OBJECT.to_string()))?
        .get(key)
        .cloned()
        .pipe(Ok)
}

pub fn set_json_value<P: AsRef<Path>, T: Serialize>(path: P, key: &str, val: T) -> Result<(), Error> {
    let mut json = read_json_object(&path)?;
    _ = json
        .as_object_mut()
        .ok_or_else(|| Error::NotJsonObject(NOT_JSON_OBJECT.to_string()))?
        .insert(key.to_string(), to_value(val).map_err(Error::from)?);
    write_json_object(path, &json)
}
