pub mod mobile;

use dioxus::core::Subscribers;
use dioxus::prelude::*;
use serde::Serialize;
use serde::de::DeserializeOwned;

/// A signal wrapper that automatically persists its value to storage
/// on every mutation. Works identically on all platforms:
/// - **wasm**: Delegates to `dioxus-sdk` localStorage (persistence handled by SDK)
/// - **mobile/desktop**: Persists to filesystem JSON via `update_key`
///
/// On wasm, the persist is a no-op since `dioxus-sdk` handles it.
/// On mobile/desktop, every `with_mut` or `set` call writes to disk.
pub struct PersistentSignal<T> {
    signal: Signal<T>,
    key: &'static str,
}

impl<T> PersistentSignal<T> {
    pub fn new(signal: Signal<T>, key: &'static str) -> Self {
        Self { signal, key }
    }
}

impl<T> Clone for PersistentSignal<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for PersistentSignal<T> {}

impl<T: 'static> PartialEq for PersistentSignal<T> {
    fn eq(&self, other: &Self) -> bool {
        self.signal == other.signal && self.key == other.key
    }
}

impl<T: 'static> Eq for PersistentSignal<T> {}

impl<T: 'static> Readable for PersistentSignal<T> {
    type Target = T;
    type Storage = UnsyncStorage;

    fn try_read_unchecked(&self) -> Result<ReadableRef<'static, Self>, BorrowError> {
        self.signal.try_read_unchecked()
    }

    fn try_peek_unchecked(&self) -> Result<ReadableRef<'static, Self>, BorrowError> {
        self.signal.try_peek_unchecked()
    }

    fn subscribers(&self) -> Subscribers {
        self.signal.subscribers()
    }
}

impl<T: Serialize + 'static> PersistentSignal<T> {
    pub fn with_mut<O>(&mut self, f: impl FnOnce(&mut T) -> O) -> O {
        let result = self.signal.with_mut(f);
        self.persist();
        result
    }

    pub fn set(&mut self, value: T) {
        self.signal.set(value);
        self.persist();
    }

    fn persist(&self) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            use dioxus::prelude::ReadableExt;
            persist_value(self.key, &*self.signal.read());
        }
    }
}

fn persist_value<T: Serialize>(key: &str, value: &T) {
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
    #[cfg(target_arch = "wasm32")]
    {
        let signal =
            dioxus_sdk::storage::use_synced_storage::<dioxus_sdk::storage::LocalStorage, T>(key.to_string(), init);
        PersistentSignal::new(signal, key)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let default = init();
        let signal = mobile::use_storage(key, || default.clone()).unwrap_or_else(|e| {
            tracing::error!("Storage init error: {}", e);
            Signal::new(default)
        });
        PersistentSignal::new(signal, key)
    }
}
