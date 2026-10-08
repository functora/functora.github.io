pub use functora_core::storage::{
    find_or_init_key, get_json_value, read_json_object, set_json_value, update_key,
    write_json_object,
};

use crate::error::Error;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy)]
pub struct Storage {
    app: &'static str,
}

impl Storage {
    #[must_use]
    pub fn new(app: &'static str) -> Self {
        Self { app }
    }

    #[must_use]
    pub fn app(&self) -> &'static str {
        self.app
    }

    #[cfg(target_os = "android")]
    pub fn files_dir(&self) -> Result<PathBuf, Error> {
        crate::platform::android::files_dir()
    }

    #[cfg(all(not(target_os = "android"), target_arch = "wasm32"))]
    pub fn files_dir(&self) -> Result<PathBuf, Error> {
        Err(Error::JS("files_dir not available on wasm".into()))
    }

    #[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
    pub fn files_dir(&self) -> Result<PathBuf, Error> {
        app_data_dir(self.app)
    }

    #[must_use]
    pub fn load<T: DeserializeOwned>(&self, key: &str) -> Option<T> {
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
            desktop_load(self.app, key)
        }
    }

    pub fn persist<T: Serialize>(&self, key: &str, value: &T) {
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
            desktop_persist(self.app, key, value);
        }
    }

    pub fn persistent<T>(&self, key: &'static str, default: T) -> Persistent<T>
    where
        T: Serialize + DeserializeOwned + Clone,
    {
        let value = self.load(key).unwrap_or(default);
        Persistent {
            storage: *self,
            key,
            value,
        }
    }
}

#[cfg(target_os = "android")]
pub fn files_dir_for(_app: &str) -> Result<PathBuf, Error> {
    crate::platform::android::files_dir()
}

#[cfg(all(not(target_os = "android"), target_arch = "wasm32"))]
pub fn files_dir_for(_app: &str) -> Result<PathBuf, Error> {
    Err(Error::JS("files_dir not available on wasm".into()))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn files_dir_for(app: &str) -> Result<PathBuf, Error> {
    app_data_dir(app)
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

#[cfg(target_arch = "wasm32")]
fn web_load<T: DeserializeOwned>(key: &str) -> Option<T> {
    let raw = crate::platform::web::storage_get(key)?;
    serde_json::from_str(&raw)
        .inspect_err(|e| tracing::warn!("Storage parse error for key {key}: {e}"))
        .ok()
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
    crate::platform::android::files_dir().ok().and_then(|p| {
        let json = read_json_object(p.join("storage.json")).ok()?;
        let value = json.get(key)?;
        serde_json::from_value(value.clone())
            .inspect_err(|e| tracing::warn!("Storage parse error for key {key}: {e}"))
            .ok()
    })
}

#[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
fn android_persist<T: Serialize>(key: &str, value: &T) {
    if let Ok(path) = crate::platform::android::files_dir().map(|p| p.join("storage.json"))
        && let Err(e) = update_key(&path, key, value)
    {
        tracing::error!("Storage persist error: {e}");
    }
}

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
fn desktop_load<T: DeserializeOwned>(app: &str, key: &str) -> Option<T> {
    storage_file_for(app).ok().and_then(|p| {
        let json = read_json_object(&p).ok()?;
        let value = json.get(key)?;
        serde_json::from_value(value.clone())
            .inspect_err(|e| tracing::warn!("Storage parse error for key {key}: {e}"))
            .ok()
    })
}

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
fn desktop_persist<T: Serialize>(app: &str, key: &str, value: &T) {
    if let Ok(path) = storage_file_for(app)
        && let Err(e) = update_key(&path, key, value)
    {
        tracing::error!("Storage persist error: {e}");
    }
}

#[derive(Debug)]
pub struct Persistent<T> {
    storage: Storage,
    key: &'static str,
    value: T,
}

impl<T> Persistent<T>
where
    T: Serialize + DeserializeOwned + Clone,
{
    #[must_use]
    pub fn get(&self) -> &T {
        &self.value
    }

    pub fn set(&mut self, value: T) {
        self.value = value;
        self.storage.persist(self.key, &self.value);
    }

    pub fn update(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.value);
        self.storage.persist(self.key, &self.value);
    }

    #[must_use]
    pub fn into_inner(self) -> T {
        self.value
    }
}

#[cfg(all(
    any(target_arch = "wasm32", target_os = "android"),
    any(feature = "web", feature = "android")
))]
#[must_use]
pub fn load_from_eframe<T: DeserializeOwned>(
    storage: Option<&dyn eframe::Storage>,
    app: &'static str,
    key: &str,
) -> Option<T> {
    storage
        .and_then(|s| s.get_string(key))
        .and_then(|raw| {
            serde_json::from_str(&raw)
                .inspect_err(|e| tracing::warn!("Storage parse error for key {key}: {e}"))
                .ok()
        })
        .or_else(|| Storage::new(app).load(key))
}

#[cfg(all(
    any(target_arch = "wasm32", target_os = "android"),
    any(feature = "web", feature = "android")
))]
pub fn save_to_eframe<T: Serialize>(
    storage: &mut dyn eframe::Storage,
    app: &'static str,
    key: &str,
    value: &T,
) {
    if let Ok(json) = serde_json::to_string(value) {
        storage.set_string(key, json.clone());
        Storage::new(app).persist(key, value);
    }
}
