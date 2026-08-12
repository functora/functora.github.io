use crate::error::Error;
use std::sync::mpsc::{Sender, channel};

pub(crate) fn jni_dispatch<T: Send + 'static>(
    f: impl FnOnce(&mut jni::JNIEnv, &jni::objects::JObject) -> Result<T, jni::errors::Error> + Send + 'static,
) -> Result<T, Error> {
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut jni::JNIEnv, activity: &jni::objects::JObject, _| {
        let res = f(env, activity);
        finish(env, res, tx);
    });
    rx.recv()?
}

pub(crate) fn jni_dispatch_with_webview<T: Send + 'static>(
    f: impl FnOnce(&mut jni::JNIEnv, &jni::objects::JObject, &jni::objects::JObject) -> Result<T, jni::errors::Error>
    + Send
    + 'static,
) -> Result<T, Error> {
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(
        move |env: &mut jni::JNIEnv, activity: &jni::objects::JObject, webview: &jni::objects::JObject| {
            let res = f(env, activity, webview);
            finish(env, res, tx);
        },
    );
    rx.recv()?
}

fn finish<T>(env: &mut jni::JNIEnv, res: Result<T, jni::errors::Error>, tx: Sender<Result<T, Error>>) {
    if let Err(ref _msg) = res {
        _ = env.exception_describe();
    }
    _ = env.exception_clear();
    _ = tx.send(res.map_err(Error::from));
}
