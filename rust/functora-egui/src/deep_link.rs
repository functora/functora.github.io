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

#[cfg(all(target_os = "android", feature = "platform"))]
#[unsafe(no_mangle)]
pub extern "system" fn Java_dev_dioxus_main_MainActivity_handleDeepLink(
    mut env: jni::JNIEnv,
    _class: jni::objects::JClass,
    url: jni::objects::JString,
) {
    if let Ok(s) = env.get_string(&url).map(String::from) {
        store_url(s);
    }
}

#[cfg(all(target_os = "android", feature = "platform"))]
#[unsafe(no_mangle)]
pub extern "system" fn Java_com_functora_app_MainActivity_handleDeepLink(
    mut env: jni::JNIEnv,
    _class: jni::objects::JClass,
    url: jni::objects::JString,
) {
    if let Ok(s) = env.get_string(&url).map(String::from) {
        store_url(s);
    }
}
