use crate::error::{Error, WorkerStopped};
use dioxus::core::Subscribers;
use dioxus::prelude::*;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::Value;
use serde_json::from_str;
use serde_json::from_value;
use serde_json::to_string_pretty;
use serde_json::to_value;
use std::fs::{OpenOptions, read_to_string, write};
use std::ops::Deref;
use std::path::Path;
use std::sync::Mutex;

/// Serializes read-modify-write cycles on the storage file so concurrent persist
/// tasks cannot lose each other's key updates or read a torn half-written file.
static STORAGE_LOCK: Mutex<()> = Mutex::new(());

#[cfg(target_os = "android")]
pub use crate::android::files_dir;

#[cfg(not(target_os = "android"))]
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
    let _guard = STORAGE_LOCK.lock().map_err(|_| Error::Worker(WorkerStopped))?;
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
    if let Some(obj) = json.as_object_mut() {
        _ = obj.insert(key.to_string(), value);
        write_json_object(path, &json)
    } else {
        Err(Error::NotJsonObject(json))
    }
}

pub struct PersistentSignal<T: 'static> {
    store: Store<T>,
    key: &'static str,
}

impl<T: 'static> PersistentSignal<T> {
    #[must_use]
    pub fn new(store: Store<T>, key: &'static str) -> Self {
        Self { store, key }
    }
}

impl<T: 'static> Clone for PersistentSignal<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Copy for PersistentSignal<T> {}

impl<T: 'static> PartialEq for PersistentSignal<T> {
    fn eq(&self, other: &Self) -> bool {
        self.store == other.store && self.key == other.key
    }
}

impl<T: 'static> Eq for PersistentSignal<T> {}

impl<T: 'static> Deref for PersistentSignal<T> {
    type Target = Store<T>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl<T: 'static> Readable for PersistentSignal<T> {
    type Target = T;
    type Storage = UnsyncStorage;

    fn try_read_unchecked(&self) -> Result<ReadableRef<'static, Self>, BorrowError> {
        self.store.try_read_unchecked()
    }

    fn try_peek_unchecked(&self) -> Result<ReadableRef<'static, Self>, BorrowError> {
        self.store.try_peek_unchecked()
    }

    fn subscribers(&self) -> Subscribers {
        self.store.subscribers()
    }
}

impl<T: Serialize + 'static> PersistentSignal<T> {
    pub fn with_mut<O>(&mut self, f: impl FnOnce(&mut T) -> O) -> O {
        let result = self.store.with_mut(f);
        self.persist();
        result
    }

    pub fn set(&mut self, value: T) {
        self.store.set(value);
        self.persist();
    }

    fn persist(&self) {
        persist_value(self.key, &*self.store.read());
    }
}

#[must_use]
pub fn load_state<T: DeserializeOwned>(key: &str) -> Option<T> {
    #[cfg(target_arch = "wasm32")]
    {
        web_sys::window()
            .and_then(|w| w.local_storage().ok()?)
            .and_then(|s| s.get_item(key).ok()?)
            .and_then(|v| serde_json::from_str(&v).ok())
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        files_dir().ok().and_then(|p| {
            let json = read_json_object(p.join("storage.json")).ok()?;
            let value = json.get(key)?;
            serde_json::from_value(value.clone()).ok()
        })
    }
}

pub fn persist_value<T: Serialize>(key: &str, value: &T) {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(window) = web_sys::window()
            && let Ok(Some(storage)) = window.local_storage()
            && let Ok(json) = serde_json::to_string(value)
            && let Err(e) = storage.set_item(key, &json)
        {
            tracing::warn!("Storage persist error: {e:?}");
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        if let Ok(path) = files_dir().map(|p| p.join("storage.json"))
            && let Err(e) = update_key(&path, key, value)
        {
            tracing::error!("Storage persist error: {}", e);
        }
    }
}

pub fn use_storage<
    T: Serialize + DeserializeOwned + Clone + std::marker::Send + std::marker::Sync + PartialEq + 'static,
>(
    key: &'static str,
    init: impl FnOnce() -> T,
) -> PersistentSignal<T> {
    let store = use_store(move || load_state(key).unwrap_or_else(init));
    let signal = PersistentSignal::new(store, key);

    let _ = use_effect(move || {
        let value = signal();
        let _ = spawn(async move {
            persist_value(key, &value);
        });
    });

    signal
}
