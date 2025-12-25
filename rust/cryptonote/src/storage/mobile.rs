use derive_more::Display;
use dioxus::{logger::tracing, prelude::*};
use serde::de::DeserializeOwned;
use serde_json::{
    Value, from_str, from_value, to_string_pretty, to_value,
};
use std::{
    fs::{OpenOptions, read_to_string, write},
    path::{Path, PathBuf},
    sync::OnceLock,
};
use tap::Pipe;

#[derive(Debug, Clone, Display)]
pub enum StorageError {
    Io(String),
    Jni(String),
    Json(String),
    Env(std::env::VarError),
    Recv(std::sync::mpsc::RecvError),
    Custom(String),
}

impl std::error::Error for StorageError {}

impl From<std::io::Error> for StorageError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e.to_string())
    }
}

#[cfg(target_os = "android")]
impl From<jni::errors::Error> for StorageError {
    fn from(e: jni::errors::Error) -> Self {
        Self::Jni(e.to_string())
    }
}

impl From<serde_json::Error> for StorageError {
    fn from(e: serde_json::Error) -> Self {
        Self::Json(e.to_string())
    }
}

impl From<std::env::VarError> for StorageError {
    fn from(e: std::env::VarError) -> Self {
        Self::Env(e)
    }
}

impl From<std::sync::mpsc::RecvError> for StorageError {
    fn from(e: std::sync::mpsc::RecvError) -> Self {
        Self::Recv(e)
    }
}

static STORAGE: OnceLock<Result<PathBuf, StorageError>> =
    OnceLock::new();

pub fn storage_path() -> Result<PathBuf, StorageError> {
    STORAGE
        .get_or_init(|| {
            files_dir()?.join("storage.json").pipe(|path| {
                OpenOptions::new()
                    .read(true)
                    .write(true)
                    .create(true)
                    .truncate(false)
                    .open(&path)?
                    .pipe(|file| {
                        if file
                            .metadata()
                            .map(|meta| meta.len() == 0)
                            .unwrap_or_default()
                        {
                            write(&path, b"{}")
                        } else {
                            Ok(())
                        }
                    })?
                    .pipe(|()| Ok(path))
            })
        })
        .clone()
}

pub fn update_key<P: AsRef<Path>, T: serde::Serialize>(
    path: P,
    key: &str,
    val: T,
) -> Result<(), StorageError> {
    read_to_string(&path)?
        .pipe(|s| from_str::<Value>(&s))?
        .pipe(|mut json| {
            json.as_object_mut()
                .ok_or_else(|| {
                    StorageError::Custom(
                        "Storage JSON is not an object"
                            .to_string(),
                    )
                })?
                .pipe(|obj| {
                    to_value(val).map(|v| {
                        obj.insert(key.to_string(), v);
                    })
                })?
                .pipe(|()| to_string_pretty(&json))?
                .pipe(|s| write(path, s))?
                .pipe(Ok)
        })
}

pub fn find_or_init_key<
    P: AsRef<Path>,
    T: DeserializeOwned + Clone + serde::Serialize,
    F: FnOnce() -> T,
>(
    path: P,
    key: &str,
    init: F,
) -> Result<T, StorageError> {
    read_to_string(&path)?
        .pipe(|s| from_str::<Value>(&s))?
        .pipe(|json| {
            json.get(key)
                .map(|val| {
                    from_value::<T>(val.clone())?.pipe(Ok)
                })
                .unwrap_or_else(|| {
                    let val = init();
                    update_key(&path, key, val.clone())
                        .map(|()| val)
                })
        })
}

pub fn use_storage<
    T: serde::Serialize + DeserializeOwned + Clone + 'static,
>(
    key: String,
    init: impl FnOnce() -> T,
) -> Result<Signal<T>, StorageError> {
    let path = storage_path()?;
    let signal =
        Signal::new(find_or_init_key(&path, &key, init)?);

    use_effect(move || {
        update_key(&path, &key, signal.read().clone())
            .unwrap_or_else(|e| {
                tracing::error!(
                    "Storage update error: {}",
                    e
                )
            })
    });

    Ok(signal)
}

#[cfg(target_os = "android")]
pub fn files_dir() -> Result<PathBuf, StorageError> {
    use jni::JNIEnv;
    use jni::objects::{JObject, JString};
    use std::sync::mpsc::channel;

    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(
        move |env: &mut JNIEnv, activity: &JObject, _| {
            env.call_method(
                activity,
                "getFilesDir",
                "()Ljava/io/File;",
                &[],
            )
            .and_then(|v| v.l())
            .and_then(|f| {
                env.call_method(
                    f,
                    "getAbsolutePath",
                    "()Ljava/lang/String;",
                    &[],
                )
            })
            .and_then(|v| v.l())
            .map(JString::from)
            .and_then(|s| {
                env.get_string(&s).map(String::from)
            })
            .map(PathBuf::from)
            .map_err(StorageError::from)
            .pipe(|res| {
                tx.send(res).unwrap_or_else(|e| {
                    tracing::error!(
                        "Storage channel error: {}",
                        e
                    )
                })
            })
        },
    );

    rx.recv()?
}

#[cfg(target_os = "ios")]
pub fn files_dir() -> Result<PathBuf, StorageError> {
    std::env::var("HOME")
        .map(|path| PathBuf::from(path).join("Documents"))?
        .pipe(Ok)
}

#[cfg(not(any(target_os = "android", target_os = "ios")))]
pub fn files_dir() -> Result<PathBuf, StorageError> {
    std::env::current_dir()?.pipe(Ok)
}
