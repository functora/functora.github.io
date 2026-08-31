use crate::archive::ArchiveSource;
use std::sync::Mutex;

pub use functora_egui::deep_link::{poll_deep_link, set_schedule_update, store_url, take_url, url_to_route};

static PENDING_ARCHIVE: Mutex<Option<ArchiveSource>> = Mutex::new(None);

pub fn store_archive(source: ArchiveSource) {
    if let Ok(mut guard) = PENDING_ARCHIVE.lock() {
        *guard = Some(source);
    }
    functora_egui::deep_link::trigger_update();
}

pub fn take_archive() -> Option<ArchiveSource> {
    PENDING_ARCHIVE.lock().ok().and_then(|mut guard| guard.take())
}

#[cfg(target_os = "android")]
#[unsafe(no_mangle)]
pub extern "system" fn Java_dev_dioxus_main_MainActivity_handleDeepLinkFile<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    path: jni::objects::JString<'local>,
) {
    if let Ok(raw) = env.get_string(&path) {
        store_archive(ArchiveSource::Path(String::from(raw).into()));
    }
}

#[cfg(target_os = "android")]
#[unsafe(no_mangle)]
pub extern "system" fn Java_com_functora_app_MainActivity_handleDeepLinkFile<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    path: jni::objects::JString<'local>,
) {
    if let Ok(raw) = env.get_string(&path) {
        store_archive(ArchiveSource::Path(String::from(raw).into()));
    }
}
