pub use functora_core::deep_link::{
    set_schedule_update, store_url, take_url, trigger_update, url_to_route,
};

#[cfg(target_arch = "wasm32")]
#[must_use]
pub fn initial_url() -> Option<String> {
    web_sys::window()?.location().href().ok()
}

#[cfg(target_os = "android")]
#[must_use]
pub fn initial_url() -> Option<String> {
    take_url()
}

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[must_use]
pub const fn initial_url() -> Option<String> {
    None
}
