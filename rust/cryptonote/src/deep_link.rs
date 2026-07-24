#[cfg(target_os = "android")]
use std::sync::mpsc::channel;
use std::sync::Mutex;

static PENDING_URL: Mutex<Option<String>> = Mutex::new(None);

pub fn store_url(url: String) {
    if let Ok(mut guard) = PENDING_URL.lock() {
        *guard = Some(url);
    }
}

pub fn take_url() -> Option<String> {
    PENDING_URL.lock().ok().and_then(|mut guard| guard.take())
}

pub fn url_to_route(url: &str) -> Option<String> {
    let query = url.split('?').nth(1)?;
    let params = query.split('&').collect::<Vec<_>>();
    if params.iter().any(|p| p.starts_with("screen=")) {
        Some(format!("/?{}", query))
    } else {
        Some(format!("/?screen=view&note={}", query.split('=').nth(1)?))
    }
}

#[cfg(target_os = "android")]
fn read_intent_url(
    env: &mut jni::JNIEnv,
    activity: &jni::objects::JObject,
) -> Result<Option<String>, jni::errors::Error> {
    let intent = env
        .call_method(activity, "getIntent", "()Landroid/content/Intent;", &[])?
        .l()?;
    let action = env
        .call_method(&intent, "getAction", "()Ljava/lang/String;", &[])?
        .l()?;
    if action.is_null() {
        return Ok(None);
    }
    let action = jni::objects::JString::from(action);
    let action_str = env.get_string(&action)?;
    if String::from(action_str) != "android.intent.action.VIEW" {
        return Ok(None);
    }
    let uri = env.call_method(&intent, "getData", "()Landroid/net/Uri;", &[])?.l()?;
    if uri.is_null() {
        return Ok(None);
    }
    let url_str = env
        .call_method(&uri, "toString", "()Ljava/lang/String;", &[])?
        .l()?;
    let jstr = jni::objects::JString::from(url_str);
    let url = env.get_string(&jstr)?;
    Ok(Some(url.into()))
}

#[cfg(target_os = "android")]
pub async fn check_intent() -> Option<String> {
    take_url().or_else(|| {
        let (tx, rx) = channel();
        dioxus::mobile::wry::prelude::dispatch(
            move |env: &mut jni::JNIEnv, activity: &jni::objects::JObject, _| {
                _ = tx.send(read_intent_url(env, activity).ok().flatten());
            },
        );
        rx.recv().ok().flatten()
    })
}

#[cfg(not(target_os = "android"))]
pub async fn check_intent() -> Option<String> {
    take_url()
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
