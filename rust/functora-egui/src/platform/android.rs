use crate::error::Error;
use jni::{
    JavaVM,
    objects::{JObject, JString},
    signature::ReturnType,
};
use std::sync::{Mutex, PoisonError};

use android_activity::AndroidApp;

static APP: Mutex<Option<AndroidApp>> = Mutex::new(None);

pub fn store_app(app: AndroidApp) {
    _ = APP
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .replace(app);
}

fn with_app<F, T>(f: F) -> Result<T, Error>
where
    F: FnOnce(&mut jni::JNIEnv, &JObject) -> Result<T, jni::errors::Error>,
{
    let guard = APP.lock().unwrap_or_else(PoisonError::into_inner);
    let app = guard
        .as_ref()
        .ok_or_else(|| Error::JS("No AndroidApp".into()))?;
    let vm = unsafe { JavaVM::from_raw(app.vm_as_ptr().cast()) }?;
    let mut env = vm.attach_current_thread()?;
    let activity: JObject = unsafe { JObject::from_raw(app.activity_as_ptr().cast()) };
    f(&mut env, &activity).map_err(Error::from)
}

pub fn files_dir() -> Result<std::path::PathBuf, Error> {
    with_app(|env, activity| {
        env.call_method(activity, "getFilesDir", "()Ljava/io/File;", &[])
            .and_then(|v| v.l())
            .and_then(|f| env.call_method(f, "getAbsolutePath", "()Ljava/lang/String;", &[]))
            .and_then(|v| v.l())
            .map(JString::from)
            .and_then(|s| env.get_string(&s).map(String::from))
            .map(std::path::PathBuf::from)
    })
}

pub async fn clipboard_read() -> Result<String, Error> {
    with_app(|env, activity| {
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip = env
            .call_method(
                clipboard.as_obj(),
                "getPrimaryClip",
                "()Landroid/content/ClipData;",
                &[],
            )?
            .l()?;
        let item = env
            .call_method(
                &clip,
                "getItemAt",
                "(I)Landroid/content/ClipData$Item;",
                &[jni::objects::JValue::Int(0)],
            )?
            .l()?;
        let text_obj = env
            .call_method(&item, "getText", "()Ljava/lang/CharSequence;", &[])?
            .l()?;
        let s = JString::from(text_obj);
        Ok(env.get_string(&s).map(String::from)?)
    })
}

pub async fn clipboard_write(text: String) -> Result<(), Error> {
    with_app(move |env, activity| {
        let label: JString = env.new_string("Cryptonote")?;
        let jtext: JString = env.new_string(&text)?;
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip_data = env
            .call_static_method(
                "android/content/ClipData",
                "newPlainText",
                "(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Landroid/content/ClipData;",
                &[(&label).into(), (&jtext).into()],
            )?
            .l()?;
        let _ = env.call_method(
            clipboard.as_obj(),
            "setPrimaryClip",
            "(Landroid/content/ClipData;)V",
            &[(&clip_data).into()],
        )?;
        Ok(())
    })
}

#[derive(Debug, Clone)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn share(data: ShareData) -> Result<(), Error> {
    with_app(move |env, activity| {
        let intent_class = env.find_class("android/content/Intent")?;
        let action_send = env
            .get_static_field(&intent_class, "ACTION_SEND", "Ljava/lang/String;")?
            .l()?;
        let intent = env.new_object(
            &intent_class,
            "(Ljava/lang/String;)V",
            &[(&action_send).into()],
        )?;
        let intent = env.new_global_ref(intent)?;
        let text_type = env.new_string("text/plain")?;
        let _ = env.call_method(
            intent.as_obj(),
            "setType",
            "(Ljava/lang/String;)Landroid/content/Intent;",
            &[(&text_type).into()],
        )?;
        let extra_text = env
            .get_static_field(&intent_class, "EXTRA_TEXT", "Ljava/lang/String;")?
            .l()?;
        let share_text = env.new_string(format!("{}\n{}", data.text, data.url))?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_text).into(), (&share_text).into()],
        )?;
        let extra_subject = env
            .get_static_field(&intent_class, "EXTRA_SUBJECT", "Ljava/lang/String;")?
            .l()?;
        let title = env.new_string(&data.title)?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_subject).into(), (&title).into()],
        )?;
        let chooser_title = env.new_string("Share via")?;
        let chooser = env.call_static_method(
            "android/content/Intent",
            "createChooser",
            "(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;",
            &[(&intent.as_obj()).into(), (&chooser_title).into()],
        )?;
        let chooser = chooser.l()?;
        let flags = env
            .get_static_field(&intent_class, "FLAG_ACTIVITY_NEW_TASK", "I")?
            .i()?;
        let _ = env.call_method(
            intent.as_obj(),
            "setFlags",
            "(I)Landroid/content/Intent;",
            &[jni::objects::JValue::Int(flags)],
        )?;
        let _ = env.call_method(
            activity,
            "startActivity",
            "(Landroid/content/Intent;)V",
            &[(&chooser).into()],
        )?;
        Ok(())
    })
}

pub async fn print_page() -> Result<(), Error> {
    Err(Error::JS(
        "Print not supported on Android without WebView".into(),
    ))
}

pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    save_to_downloads(&data, filename.to_string(), |_, _| {})?;
    Ok(filename.to_string())
}

pub fn save_to_downloads(
    data: &[u8],
    name: String,
    mut report: impl FnMut(u64, u64),
) -> Result<(), Error> {
    const CHUNK: usize = 4 * 1024 * 1024;
    let total = data.len() as u64;
    report(0, total);
    with_app(|env, activity| {
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
        let jn = env.new_string(&name)?;
        let _ = env.call_method(
            &cv,
            "put",
            "(Ljava/lang/String;Ljava/lang/String;)V",
            &[(&nk).into(), (&jn).into()],
        )?;
        let mk = env.new_string("mime_type")?;
        let mv = mime_for(env, &name)?;
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
        let os = env
            .call_method(
                &resolver,
                "openOutputStream",
                "(Landroid/net/Uri;)Ljava/io/OutputStream;",
                &[(&uri).into()],
            )?
            .l()?;
        let mut done = 0u64;
        for chunk in data.chunks(CHUNK) {
            let ba = env.byte_array_from_slice(chunk)?;
            let _ = env.call_method(&os, "write", "([B)V", &[(&ba).into()])?;
            done += chunk.len() as u64;
            report(done, total);
        }
        let _ = env.call_method(&os, "close", "()V", &[])?;
        Ok(())
    })
}

fn mime_for<'local>(
    env: &mut jni::JNIEnv<'local>,
    name: &str,
) -> Result<jni::objects::JString<'local>, jni::errors::Error> {
    let ext = name
        .rsplit_once('.')
        .map(|(_, e)| e.to_lowercase())
        .unwrap_or_default();
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

pub fn get_data_string() -> Option<String> {
    let guard = APP.lock().unwrap_or_else(PoisonError::into_inner);
    let app = guard.as_ref()?;
    let vm = unsafe { JavaVM::from_raw(app.vm_as_ptr().cast()) }.ok()?;
    let mut env = vm.attach_current_thread().ok()?;
    let activity: JObject = unsafe { JObject::from_raw(app.activity_as_ptr().cast()) };
    let activity_cls = env.get_object_class(&activity).ok()?;
    let get_intent = env
        .get_method_id(&activity_cls, "getIntent", "()Landroid/content/Intent;")
        .ok()?;
    let intent =
        unsafe { env.call_method_unchecked(&activity, get_intent, ReturnType::Object, &[]) }
            .ok()?
            .l()
            .ok()?;
    if intent.is_null() {
        return None;
    }
    let intent_cls = env.get_object_class(&intent).ok()?;
    let get_data = env
        .get_method_id(&intent_cls, "getDataString", "()Ljava/lang/String;")
        .ok()?;
    let data = unsafe { env.call_method_unchecked(&intent, get_data, ReturnType::Object, &[]) }
        .ok()?
        .l()
        .ok()?;
    if data.is_null() {
        return None;
    }
    let jstring = JString::from(data);
    let url = env.get_string(&jstring).ok()?;
    Some(String::from(url))
}

pub async fn sleep(millis: u64) {
    std::thread::sleep(std::time::Duration::from_millis(millis));
}
