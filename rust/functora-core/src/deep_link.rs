use std::sync::{Arc, Mutex, PoisonError};

static PENDING_URL: Mutex<Option<String>> = Mutex::new(None);
static SCHEDULE_UPDATE: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

pub fn store_url(url: String) {
    *PENDING_URL.lock().unwrap_or_else(PoisonError::into_inner) = Some(url);
    trigger_update();
}

pub fn set_schedule_update(f: Arc<dyn Fn() + Send + Sync>) {
    *SCHEDULE_UPDATE
        .lock()
        .unwrap_or_else(PoisonError::into_inner) = Some(f);
}

pub fn trigger_update() {
    SCHEDULE_UPDATE
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .as_ref()
        .cloned()
        .into_iter()
        .for_each(|update| update());
}

pub fn take_url() -> Option<String> {
    PENDING_URL
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .take()
}

#[must_use]
pub fn url_to_route(url: &str) -> Option<String> {
    url.split('?').nth(1).map(|query| format!("/?{query}"))
}
