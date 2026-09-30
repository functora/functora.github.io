pub use functora_core::deep_link::{
    set_schedule_update, store_url, take_url, trigger_update, url_to_route,
};

#[cfg(all(target_os = "android", feature = "platform"))]
#[must_use]
pub fn poll_deep_link() -> Option<String> {
    crate::platform::android::get_data_string().or_else(take_url)
}

#[cfg(not(all(target_os = "android", feature = "platform")))]
#[must_use]
pub fn poll_deep_link() -> Option<String> {
    take_url()
}

#[cfg(all(target_arch = "wasm32", feature = "platform"))]
pub fn poll_deep_link_web() -> Option<String> {
    crate::platform::web::location_href()
        .and_then(|href| {
            if href.contains('?') || href.contains('#') {
                Some(href)
            } else {
                None
            }
        })
        .or_else(take_url)
}
