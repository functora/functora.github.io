pub mod mobile;

pub fn use_storage<
    T: serde::Serialize
        + serde::de::DeserializeOwned
        + Clone
        + std::marker::Send
        + std::marker::Sync
        + PartialEq
        + 'static,
>(
    key: &'static str,
    init: impl FnOnce() -> T,
) -> dioxus::prelude::Signal<T> {
    #[cfg(target_arch = "wasm32")]
    {
        dioxus_sdk::storage::use_synced_storage::<dioxus_sdk::storage::LocalStorage, T>(key.to_string(), init)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let default = init();
        mobile::use_storage(key, || default.clone()).unwrap_or_else(|e| {
            tracing::error!("Storage init error: {}", e);
            dioxus::prelude::Signal::new(default)
        })
    }
}
