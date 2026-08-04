use std::sync::Arc;
use std::sync::Mutex;

static PENDING_URL: Mutex<Option<String>> = Mutex::new(None);
static PENDING_ARCHIVE: Mutex<Option<Vec<u8>>> = Mutex::new(None);
static SCHEDULE_UPDATE: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

pub fn store_url(url: String) {
    if let Ok(mut guard) = PENDING_URL.lock() {
        *guard = Some(url);
    }
    if let Some(update) = SCHEDULE_UPDATE.lock().ok().and_then(|guard| guard.as_ref().cloned()) {
        update();
    }
}

pub fn set_schedule_update(f: Arc<dyn Fn() + Send + Sync>) {
    if let Ok(mut guard) = SCHEDULE_UPDATE.lock() {
        *guard = Some(f);
    }
}

pub fn take_url() -> Option<String> {
    PENDING_URL.lock().ok().and_then(|mut guard| guard.take())
}

pub fn store_archive(bytes: Vec<u8>) {
    if let Ok(mut guard) = PENDING_ARCHIVE.lock() {
        *guard = Some(bytes);
    }
    if let Some(update) = SCHEDULE_UPDATE.lock().ok().and_then(|guard| guard.as_ref().cloned()) {
        update();
    }
}

pub fn take_archive() -> Option<Vec<u8>> {
    PENDING_ARCHIVE.lock().ok().and_then(|mut guard| guard.take())
}

pub fn url_to_route(url: &str) -> Option<String> {
    url.split('?').nth(1).map(|query| format!("/?{}", query))
}

#[cfg(target_os = "android")]
#[no_mangle]
pub extern "system" fn Java_dev_dioxus_main_MainActivity_handleDeepLink<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    url: jni::objects::JString<'local>,
) {
    if let Ok(s) = env.get_string(&url) {
        store_url(s.into());
    }
}

#[cfg(target_os = "android")]
#[no_mangle]
pub extern "system" fn Java_dev_dioxus_main_MainActivity_handleDeepLinkFile<'local>(
    env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    bytes: jni::objects::JByteArray<'local>,
) {
    if let Ok(bytes) = env.convert_byte_array(&bytes) {
        store_archive(bytes);
    }
}
