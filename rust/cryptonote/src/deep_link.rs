use crate::archive::ArchiveSource;
use std::sync::{Mutex, PoisonError};

pub use functora_egui::deep_link::{poll_deep_link, set_schedule_update, store_url, take_url, url_to_route};

static PENDING_ARCHIVE: Mutex<Option<ArchiveSource>> = Mutex::new(None);

pub fn store_archive(source: ArchiveSource) {
    *PENDING_ARCHIVE.lock().unwrap_or_else(PoisonError::into_inner) = Some(source);
    functora_egui::deep_link::trigger_update();
}

pub fn take_archive() -> Option<ArchiveSource> {
    PENDING_ARCHIVE.lock().unwrap_or_else(PoisonError::into_inner).take()
}

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
pub fn ingest_desktop_args() {
    for path in functora_egui::desktop::take_file_args() {
        let is_archive = path
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext == "cryptonote");
        if is_archive {
            store_archive(ArchiveSource::Path(path));
        }
    }
}

#[must_use]
pub fn has_pending_archive() -> bool {
    PENDING_ARCHIVE.lock().unwrap_or_else(PoisonError::into_inner).is_some()
}

#[cfg(target_os = "android")]
#[unsafe(no_mangle)]
pub extern "system" fn Java_com_functora_cryptonote_egui_MainActivity_handleDeepLink<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    url: jni::objects::JString<'local>,
) {
    if let Ok(raw) = env.get_string(&url) {
        store_url(String::from(raw));
    }
}

#[cfg(target_os = "android")]
#[unsafe(no_mangle)]
pub extern "system" fn Java_com_functora_cryptonote_egui_MainActivity_handleDeepLinkFile<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    path: jni::objects::JString<'local>,
) {
    if let Ok(raw) = env.get_string(&path) {
        store_archive(ArchiveSource::Path(String::from(raw).into()));
    }
}
