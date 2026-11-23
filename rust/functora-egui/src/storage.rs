pub use functora_core::storage::{
    find_or_init_key, get_json_value, read_json_object, remove_key, set_json_value, update_key,
    write_json_object,
};

use crate::error::Error;
#[cfg(not(target_os = "android"))]
use crate::error::Missing;
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
        Err(Error::Missing(Missing::DataDir))
    }

    #[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
    pub fn files_dir(&self) -> Result<PathBuf, Error> {
        app_data_dir(self.app)
    }

    #[must_use]
    pub fn load<T: DeserializeOwned>(&self, key: &str) -> Option<T> {
        self.try_load(key)
            .inspect_err(|error| tracing::warn!("Storage load error for key {key}: {error}"))
            .ok()
            .flatten()
    }

    pub fn try_load<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>, Error> {
        #[cfg(target_arch = "wasm32")]
        {
            web_try_load(key)
        }
        #[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
        {
            android_try_load(key)
        }
        #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
        {
            desktop_try_load(self.app, key)
        }
    }

    pub fn persist<T: Serialize>(&self, key: &str, value: &T) {
        if let Err(error) = self.try_persist(key, value) {
            tracing::warn!("Storage persist error for key {key}: {error}");
        }
    }

    pub fn try_persist<T: Serialize>(&self, key: &str, value: &T) -> Result<(), Error> {
        #[cfg(target_arch = "wasm32")]
        {
            crate::platform::web::storage_set(key, &serde_json::to_string(value)?)
        }
        #[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
        {
            let path = crate::platform::android::files_dir()?.join("storage.json");
            update_key(path, key, value)
        }
        #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
        {
            let path = storage_file_for(self.app)?;
            update_key(path, key, value)
        }
    }

    pub fn try_remove(&self, key: &str) -> Result<(), Error> {
        #[cfg(target_arch = "wasm32")]
        {
            crate::platform::web::storage_remove(key)
        }
        #[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
        {
            let path = crate::platform::android::files_dir()?.join("storage.json");
            remove_key(path, key)
        }
        #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
        {
            let path = storage_file_for(self.app)?;
            remove_key(path, key)
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
    Err(Error::Missing(Missing::DataDir))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn files_dir_for(app: &str) -> Result<PathBuf, Error> {
    app_data_dir(app)
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn app_data_dir(app: &str) -> Result<PathBuf, Error> {
    directories::ProjectDirs::from("io", "functora", app)
        .map(|dir| dir.data_dir().to_path_buf())
        .ok_or(Error::Missing(Missing::DataDir))
}

#[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
pub fn storage_file_for(app: &str) -> Result<PathBuf, Error> {
    let dir = app_data_dir(app)?;
    std::fs::create_dir_all(&dir)?;
    Ok(dir.join("storage.json"))
}

#[cfg(target_arch = "wasm32")]
fn web_try_load<T: DeserializeOwned>(key: &str) -> Result<Option<T>, Error> {
    match crate::platform::web::storage_try_get(key)? {
        Some(raw) => serde_json::from_str(&raw).map(Some).map_err(Error::from),
        None => Ok(None),
    }
}

#[cfg(all(not(target_arch = "wasm32"), target_os = "android"))]
fn android_try_load<T: DeserializeOwned>(key: &str) -> Result<Option<T>, Error> {
    let path = crate::platform::android::files_dir()?.join("storage.json");
    if !path.exists() {
        return Ok(None);
    }
    let json = read_json_object(path)?;
    let Some(value) = json.get(key) else {
        return Ok(None);
    };
    serde_json::from_value(value.clone())
        .map(Some)
        .map_err(Error::from)
}

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
fn desktop_try_load<T: DeserializeOwned>(app: &str, key: &str) -> Result<Option<T>, Error> {
    let path = storage_file_for(app)?;
    if !path.exists() {
        return Ok(None);
    }
    let json = read_json_object(path)?;
    let Some(value) = json.get(key) else {
        return Ok(None);
    };
    serde_json::from_value(value.clone())
        .map(Some)
        .map_err(Error::from)
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
