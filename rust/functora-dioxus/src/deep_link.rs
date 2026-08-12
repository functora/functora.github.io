use std::sync::{Arc, Mutex};

static PENDING_URL: Mutex<Option<String>> = Mutex::new(None);
static SCHEDULE_UPDATE: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

pub fn store_url(url: String) {
    if let Ok(mut guard) = PENDING_URL.lock() {
        *guard = Some(url);
    }
    trigger_update();
}

pub fn set_schedule_update(f: Arc<dyn Fn() + Send + Sync>) {
    if let Ok(mut guard) = SCHEDULE_UPDATE.lock() {
        *guard = Some(f);
    }
}

pub fn trigger_update() {
    if let Some(update) = SCHEDULE_UPDATE.lock().ok().and_then(|guard| guard.as_ref().cloned()) {
        update();
    }
}

pub fn take_url() -> Option<String> {
    PENDING_URL.lock().ok().and_then(|mut guard| guard.take())
}

#[must_use]
pub fn url_to_route(url: &str) -> Option<String> {
    url.split('?').nth(1).map(|query| format!("/?{query}"))
}
