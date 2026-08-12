use crate::error::{Error, WorkerStopped};
use jni::JNIEnv;
use jni::objects::{GlobalRef, JObject};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};

const WRITE_CHUNK: usize = 4 * 1024 * 1024;

pub fn save_to_downloads(
    data: &[u8],
    name: String,
    mut report: impl FnMut(u64, u64) + Send + 'static,
) -> Result<(), Error> {
    let total = data.len() as u64;
    report(0, total);
    let stream: Arc<Mutex<Option<GlobalRef>>> = Arc::new(Mutex::new(None));
    let (tx, rx) = channel();
    let open = stream.clone();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, activity: &JObject, _| {
        let res = (|| -> Result<(), Error> {
            let obj = open_stream(env, activity, &name)?;
            let global = env.new_global_ref(obj)?;
            let mut slot = open.lock().map_err(|_| Error::Worker(WorkerStopped))?;
            *slot = Some(global);
            Ok(())
        })();
        if let Err(ref error) = res {
            tracing::error!("MediaStore JNI open error: {error}");
            _ = env.exception_describe();
        }
        _ = env.exception_clear();
        _ = tx.send(res);
    });
    rx.recv()??;
    let mut done = 0u64;
    for chunk in data.chunks(WRITE_CHUNK) {
        let (tx_chunk, rx_chunk) = channel();
        let stream_chunk = stream.clone();
        let size = chunk.len() as u64;
        let chunk_owned = chunk.to_vec();
        dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, _, _| {
            let res = (|| -> Result<(), Error> {
                let os = stream_chunk
                    .lock()
                    .map_err(|_| Error::Worker(WorkerStopped))?
                    .as_ref()
                    .cloned()
                    .ok_or(Error::Worker(WorkerStopped))?;
                let ba = env.byte_array_from_slice(&chunk_owned)?;
                let _ = env.call_method(os.as_obj(), "write", "([B)V", &[(&ba).into()])?;
                Ok(())
            })();
            if let Err(ref error) = res {
                tracing::error!("MediaStore JNI write error: {error}");
                _ = env.exception_describe();
            }
            _ = env.exception_clear();
            _ = tx_chunk.send(res);
        });
        rx_chunk.recv()??;
        done += size;
        report(done, total);
    }
    let (tx_close, rx_close) = channel();
    let stream_close = stream.clone();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, _, _| {
        let res = (|| -> Result<(), Error> {
            if let Some(os) = stream_close.lock().map_err(|_| Error::Worker(WorkerStopped))?.take() {
                let _ = env.call_method(os.as_obj(), "close", "()V", &[])?;
            }
            Ok(())
        })();
        if let Err(ref error) = res {
            tracing::error!("MediaStore JNI close error: {error}");
            _ = env.exception_describe();
        }
        _ = env.exception_clear();
        _ = tx_close.send(res);
    });
    rx_close.recv()??;
    Ok(())
}

fn open_stream<'local>(
    env: &mut JNIEnv<'local>,
    activity: &JObject,
    name: &str,
) -> Result<JObject<'local>, jni::errors::Error> {
    let resolver = env
        .call_method(
            activity,
            "getContentResolver",
            "()Landroid/content/ContentResolver;",
            &[],
        )?
        .l()?;
    let cv = env.new_object("android/content/ContentValues", "()V", &[])?;
    let nk = env.new_string("_display_name")?;
    let jn = env.new_string(name)?;
    let _ = env.call_method(
        &cv,
        "put",
        "(Ljava/lang/String;Ljava/lang/String;)V",
        &[(&nk).into(), (&jn).into()],
    )?;
    let mk = env.new_string("mime_type")?;
    let mv = mime_for(env, name)?;
    let _ = env.call_method(
        &cv,
        "put",
        "(Ljava/lang/String;Ljava/lang/String;)V",
        &[(&mk).into(), (&mv).into()],
    )?;
    let base_uri = env
        .get_static_field(
            "android/provider/MediaStore$Downloads",
            "EXTERNAL_CONTENT_URI",
            "Landroid/net/Uri;",
        )?
        .l()?;
    let uri = env
        .call_method(
            &resolver,
            "insert",
            "(Landroid/net/Uri;Landroid/content/ContentValues;)Landroid/net/Uri;",
            &[(&base_uri).into(), (&cv).into()],
        )?
        .l()?;
    env.call_method(
        &resolver,
        "openOutputStream",
        "(Landroid/net/Uri;)Ljava/io/OutputStream;",
        &[(&uri).into()],
    )?
    .l()
}

fn mime_for<'local>(env: &mut JNIEnv<'local>, name: &str) -> Result<jni::objects::JString<'local>, jni::errors::Error> {
    let ext = name.rsplit_once('.').map(|(_, e)| e.to_lowercase()).unwrap_or_default();
    let mtm = env
        .call_static_method(
            "android/webkit/MimeTypeMap",
            "getSingleton",
            "()Landroid/webkit/MimeTypeMap;",
            &[],
        )?
        .l()?;
    let jext = env.new_string(&ext)?;
    let jmime = env
        .call_method(
            &mtm,
            "getMimeTypeFromExtension",
            "(Ljava/lang/String;)Ljava/lang/String;",
            &[(&jext).into()],
        )?
        .l()?;
    if jmime.as_raw().is_null() {
        env.new_string("application/octet-stream")
    } else {
        Ok(jmime.into())
    }
}
