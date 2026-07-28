use crate::error::AppError;
use crate::messages::Msg;
use crate::*;

use base64::Engine;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>().language()()
}

pub fn use_message() -> Signal<Option<Msg>> {
    use_signal(|| None)
}

pub fn read_clipboard(on_paste: impl FnOnce(String) + 'static, mut message: Signal<Option<Msg>>) {
    spawn(async move {
        match functora_dioxus::ffi::read_clipboard().await {
            Ok(text) => on_paste(text),
            Err(e) => message.set(Some(Msg::Error(AppError::Fd(e)))),
        }
    });
}

pub fn write_clipboard(val: String, message: Signal<Option<Msg>>) {
    functora_dioxus::ffi::write_clipboard(val, message, Msg::Base(BaseMsg::Copied), |e| {
        Msg::Base(BaseMsg::ClipboardWriteError(e.to_string()))
    });
}

pub fn edit_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        tst.action().set(ActionMode::Create);
        nav.write().push(Screen::Home.to_route(None));
    }
}

pub fn reset_temporary_state(mut tst: Store<TemporaryState>) {
    tst.set(TemporaryState::default());
    // WORKAROUND: Root Store writes don't always propagate to child-lens subscribers. Force it.
    tst.action().set(ActionMode::Create);
}

pub fn reset_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        reset_temporary_state(tst);
        nav.write().push(Screen::Home.to_route(None));
    }
}

pub fn handle_file_input(evt: dioxus::prelude::FormEvent, tst: Store<TemporaryState>) {
    #[cfg(target_arch = "wasm32")]
    use dioxus::web::WebEventExt;
    #[cfg(target_arch = "wasm32")]
    use wasm_bindgen::JsCast;
    let files = evt.files();
    #[cfg(target_arch = "wasm32")]
    let input = evt
        .data()
        .as_web_event()
        .target()
        .and_then(|t| t.dyn_into::<web_sys::HtmlInputElement>().ok());
    spawn(async move {
        let mut current = tst.attachments()();
        for f in files {
            let name = f.name();
            if let Ok(bytes) = f.read_bytes().await {
                current.push(Attachment {
                    name,
                    data: bytes.to_vec(),
                });
            }
        }
        tst.attachments().set(current);
        #[cfg(target_arch = "wasm32")]
        if let Some(input) = input {
            input.set_value("");
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn handle_file_input_native(tst: Store<TemporaryState>) {
    spawn(async move {
        let mut current = tst.attachments()();
        for (name, data) in pick_files_via_eval(true).await {
            current.push(Attachment { name, data });
        }
        tst.attachments().set(current);
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn open_archive_file_native(tst: Store<TemporaryState>, mut message: Signal<Option<Msg>>) {
    spawn(async move {
        let files = pick_files_via_eval(false).await;
        let (_, bytes) = match files.into_iter().next() {
            Some(f) => f,
            None => return,
        };
        match read_archive_metadata(&bytes) {
            Ok(meta) => {
                tst.archive_bytes().set(Some(bytes));
                tst.archive_meta().set(Some(meta));
                tst.view().is_encrypted().set(true);
                tst.extracted_files().set(Vec::new());
                tst.view().password_input().set(String::new());
            }
            Err(e) => message.set(Some(Msg::Error(e))),
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
async fn pick_files_via_eval(multiple: bool) -> Vec<(String, Vec<u8>)> {
    use base64::engine::general_purpose::STANDARD as BASE64;

    let multiple_str = if multiple { "true" } else { "false" };
    let code = format!(
        r#"
        (async function() {{
            const input = document.createElement('input');
            input.type = 'file';
            input.multiple = {multiple};
            input.style.display = 'none';
            document.body.appendChild(input);
            try {{
                const files = await new Promise((resolve, reject) => {{
                    const timer = setTimeout(() => {{ input.remove(); resolve([]); }}, 120000);
                    input.addEventListener('change', () => {{
                        clearTimeout(timer);
                        Promise.all(Array.from(input.files).map(f => new Promise(r => {{
                            const reader = new FileReader();
                            reader.onload = () => r({{ name: f.name, data: reader.result.split(',')[1] }});
                            reader.onerror = () => r(null);
                            reader.readAsDataURL(f);
                        }}))).then(r => {{ input.remove(); resolve(r.filter(x => x)); }});
                    }}, {{ once: true }});
                    input.click();
                }});
                dioxus.send(files);
            }} catch(e) {{
                input.remove();
                dioxus.send([]);
            }}
        }})()
        "#,
        multiple = multiple_str
    );

    #[derive(Deserialize)]
    struct FileResult {
        name: String,
        data: String,
    }

    let mut eval = dioxus::document::eval(&code);
    match eval.recv::<Vec<FileResult>>().await {
        Ok(files) => files
            .into_iter()
            .filter_map(|f| BASE64.decode(&f.data).ok().map(|bytes| (f.name, bytes)))
            .collect(),
        Err(_) => Vec::new(),
    }
}

#[cfg(not(target_os = "android"))]
pub fn download_package(data: Vec<u8>, filename: &str) {
    let b64 = base64::engine::general_purpose::STANDARD.encode(&data);
    let script = format!(
        r#"const a=document.createElement('a');a.href="data:application/octet-stream;base64,{b64}";a.download='{filename}';a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>document.body.removeChild(a),1000);"#,
    );
    let _ = dioxus::document::eval(&script);
}

#[cfg(target_os = "android")]
pub fn download_package(data: Vec<u8>, filename: &str) {
    use jni::objects::JObject;
    use jni::JNIEnv;
    use std::sync::mpsc::channel;
    let (tx, rx) = channel();
    let filename = filename.to_string();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, activity: &JObject, _| {
        let res = (|| -> Result<(), jni::errors::Error> {
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
            let jn = env.new_string(&filename)?;
            env.call_method(
                &cv,
                "put",
                "(Ljava/lang/String;Ljava/lang/String;)V",
                &[(&nk).into(), (&jn).into()],
            )?;
            let mk = env.new_string("mime_type")?;
            let mv = env.new_string("application/octet-stream")?;
            env.call_method(
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
            let ba = env.byte_array_from_slice(&data)?;
            env.call_method(&os, "write", "([B)V", &[(&ba).into()])?;
            env.call_method(&os, "close", "()V", &[])?;
            Ok(())
        })();
        if let Err(ref e) = res {
            tracing::error!("MediaStore JNI error: {e}");
            let _ = env.exception_describe();
        }
        let _ = env.exception_clear();
        let _ = tx.send(res);
    });
    match rx.recv() {
        Ok(Ok(())) => tracing::info!("Saved to Downloads"),
        Ok(Err(e)) => tracing::error!("save failed: {e}"),
        Err(e) => tracing::error!("channel error: {e:?}"),
    }
}

pub fn remove_attachment(tst: Store<TemporaryState>, index: usize) {
    let mut atts = tst.attachments()();
    atts.remove(index);
    tst.attachments().set(atts);
}

pub fn format_size(size: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    if size >= MB {
        format!("{:.1} MB", size as f64 / MB as f64)
    } else if size >= KB {
        format!("{:.1} KB", size as f64 / KB as f64)
    } else {
        format!("{} B", size)
    }
}
