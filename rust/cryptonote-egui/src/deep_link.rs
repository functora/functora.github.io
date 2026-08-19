pub use functora_core::deep_link::{
    set_schedule_update, store_url, take_url, trigger_update, url_to_route,
};

#[cfg(target_arch = "wasm32")]
#[must_use]
pub fn initial_url() -> Option<String> {
    web_sys::window()?.location().href().ok()
}

#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub const fn initial_url() -> Option<String> {
    None
}
