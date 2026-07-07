pub mod mobile;

use dioxus::core::Subscribers;
use dioxus::prelude::*;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::ops::Deref;

pub struct PersistentSignal<T: 'static> {
    store: Store<T>,
    key: &'static str,
}

impl<T: 'static> PersistentSignal<T> {
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
        mobile::files_dir().ok().and_then(|p| {
            let json = mobile::read_json_object(p.join("storage.json")).ok()?;
            let value = json.get(key)?;
            serde_json::from_value(value.clone()).ok()
        })
    }
}

pub fn persist_value<T: Serialize>(key: &str, value: &T) {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(window) = web_sys::window() {
            if let Ok(Some(storage)) = window.local_storage() {
                if let Ok(json) = serde_json::to_string(value) {
                    let _ = storage.set_item(key, &json);
                }
            }
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        if let Ok(path) = mobile::files_dir().map(|p| p.join("storage.json"))
            && let Err(e) = mobile::update_key(&path, key, value)
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
