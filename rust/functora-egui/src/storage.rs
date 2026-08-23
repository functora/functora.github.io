pub use functora_core::storage::{
    find_or_init_key, get_json_value, read_json_object, set_json_value, update_key,
    write_json_object,
};

use crate::error::Error;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::path::PathBuf;

#[cfg(target_os = "android")]
pub fn files_dir() -> Result<PathBuf, Error> {
    crate::platform::android::files_dir()
}

#[cfg(all(not(target_os = "android"), target_arch = "wasm32"))]
pub fn files_dir() -> Result<PathBuf, Error> {
    Err(Error::JS("files_dir not available on wasm".into()))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn files_dir() -> Result<PathBuf, Error> {
    app_data_dir("functora-egui")
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn app_data_dir(app: &str) -> Result<PathBuf, Error> {
    directories::ProjectDirs::from("io", "functora", app)
        .map(|d| d.data_dir().to_path_buf())
        .ok_or_else(|| Error::JS("No data dir".into()))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn storage_file_for(app: &str) -> Result<PathBuf, Error> {
    let dir = app_data_dir(app)?;
    std::fs::create_dir_all(&dir)?;
    Ok(dir.join("storage.json"))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn storage_file() -> Result<PathBuf, Error> {
    storage_file_for("functora-egui")
}

#[must_use]
pub fn load_state<T: DeserializeOwned>(key: &str) -> Option<T> {
    #[cfg(target_arch = "wasm32")]
    {
        web_load(key)
    }
    #[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
    {
        android_load(key)
    }
    #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
    {
        desktop_load(key)
    }
}

pub fn persist_value<T: Serialize>(key: &str, value: &T) {
    #[cfg(target_arch = "wasm32")]
    {
        web_persist(key, value);
    }
    #[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
    {
        android_persist(key, value);
    }
    #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
    {
        desktop_persist(key, value);
    }
}

#[cfg(target_arch = "wasm32")]
fn web_load<T: DeserializeOwned>(key: &str) -> Option<T> {
    let raw = crate::platform::web::storage_get(key)?;
    serde_json::from_str(&raw).ok()
}

#[cfg(target_arch = "wasm32")]
fn web_persist<T: Serialize>(key: &str, value: &T) {
    if let Ok(json) = serde_json::to_string(value)
        && let Err(e) = crate::platform::web::storage_set(key, &json)
    {
        tracing::warn!("Storage persist error: {e}");
    }
}

#[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
fn android_load<T: DeserializeOwned>(key: &str) -> Option<T> {
    files_dir().ok().and_then(|p| {
        let json = read_json_object(p.join("storage.json")).ok()?;
        let value = json.get(key)?;
        serde_json::from_value(value.clone()).ok()
    })
}

#[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
fn android_persist<T: Serialize>(key: &str, value: &T) {
    if let Ok(path) = files_dir().map(|p| p.join("storage.json")) {
        if let Err(e) = update_key(&path, key, value) {
            tracing::error!("Storage persist error: {e}");
        }
    }
}

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
fn desktop_load<T: DeserializeOwned>(key: &str) -> Option<T> {
    storage_file().ok().and_then(|p| {
        let json = read_json_object(&p).ok()?;
        let value = json.get(key)?;
        serde_json::from_value(value.clone()).ok()
    })
}

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
fn desktop_persist<T: Serialize>(key: &str, value: &T) {
    if let Ok(path) = storage_file()
        && let Err(e) = update_key(&path, key, value)
    {
        tracing::error!("Storage persist error: {e}");
    }
}

#[derive(Debug)]
pub struct Persistent<T> {
    key: &'static str,
    value: T,
}

impl<T> Persistent<T>
where
    T: Serialize + DeserializeOwned + Clone,
{
    #[must_use]
    pub fn new(key: &'static str, default: T) -> Self {
        let value = load_state(key).unwrap_or(default);
        Self { key, value }
    }

    #[must_use]
    pub fn get(&self) -> &T {
        &self.value
    }

    pub fn set(&mut self, value: T) {
        self.value = value;
        persist_value(self.key, &self.value);
    }

    pub fn update(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.value);
        persist_value(self.key, &self.value);
    }

    #[must_use]
    pub fn into_inner(self) -> T {
        self.value
    }
}

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
#[must_use]
pub fn load_from_eframe<T: DeserializeOwned>(
    storage: Option<&dyn eframe::Storage>,
    key: &str,
) -> Option<T> {
    storage
        .and_then(|s| s.get_string(key))
        .and_then(|raw| serde_json::from_str(&raw).ok())
        .or_else(|| load_state(key))
}

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
pub fn save_to_eframe<T: Serialize>(storage: &mut dyn eframe::Storage, key: &str, value: &T) {
    if let Ok(json) = serde_json::to_string(value) {
        storage.set_string(key, json.clone());
        persist_value(key, value);
    }
}
