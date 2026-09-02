pub use functora_core::deep_link::{
    set_schedule_update, store_url, take_url, trigger_update, url_to_route,
};

#[cfg(target_os = "android")]
#[must_use]
pub fn poll_deep_link() -> Option<String> {
    crate::platform::android::get_data_string().or_else(take_url)
}

#[cfg(not(target_os = "android"))]
#[must_use]
pub fn poll_deep_link() -> Option<String> {
    take_url()
}

#[cfg(target_arch = "wasm32")]
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

#[cfg(target_os = "android")]
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

#[cfg(target_os = "android")]
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
