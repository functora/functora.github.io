use crate::error::AppError;
use crate::messages::Msg;
use crate::*;

use base64::Engine;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>().language()()
}

pub fn use_message_markdown<T: I18N + 'static>(msg: T) -> Memo<String> {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    use_memo(move || msg.render_markdown(pst.language()()))
}

pub fn use_message() -> Signal<Option<Msg>> {
    use_signal(|| None)
}

pub fn read_clipboard(on_paste: impl FnOnce(String) + 'static, mut message: Signal<Option<Msg>>) {
    spawn(async move {
        match functora_dioxus::ffi::read_clipboard().await {
            Ok(text) => on_paste(text),
            Err(e) => message.set(Some(Msg::Error(AppError::FunctoraDioxus(e)))),
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

// IMPORTANT! Do not remove the per-field resets below: keep them in sync with
// new TemporaryState fields. tst.set(TemporaryState::default()) is kept as a safety
// net, but dioxus-stores 0.7.2 fails to notify field subscribers on whole-store
// writes (paths_under bug, fixed in 0.7.4 via https://github.com/DioxusLabs/dioxus/pull/5069),
// so the per-field writes mark every field dirty.
pub fn reset_temporary_state(mut tst: Store<TemporaryState>) {
    tst.set(TemporaryState::default());
    tst.note().set(String::new());
    tst.password().set(String::new());
    tst.cipher().set(Some(CipherType::Aes256Gcm));
    tst.attachments().set(Vec::new());
    tst.screen().set(Screen::default());
    tst.action().set(ActionMode::Create);
    tst.url_input().set(String::new());
    tst.external().set(External::Nothing);
}

pub fn reset_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        reset_temporary_state(tst);
        nav.write().push(Screen::Home.to_route(None));
    }
}

pub fn handle_file_input(
    evt: dioxus::prelude::FormEvent,
    tst: Store<TemporaryState>,
    mut message: Signal<Option<Msg>>,
) {
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
        let mut errors = Vec::new();
        for f in files {
            let name = f.name();
            match f.read_bytes().await {
                Ok(bytes) => add_attachment(
                    &mut current,
                    Attachment {
                        name,
                        data: bytes.to_vec(),
                    },
                ),
                Err(e) => errors.push(format!("{name}: {e}")),
            }
        }
        tst.attachments().set(current);
        if let Some(first) = errors.into_iter().next() {
            message.set(Some(Msg::Error(AppError::FunctoraDioxus(functora_dioxus::Error::IO(
                first,
            )))));
        }
        #[cfg(target_arch = "wasm32")]
        if let Some(input) = input {
            input.set_value("");
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn handle_file_input_native(tst: Store<TemporaryState>, mut message: Signal<Option<Msg>>) {
    spawn(async move {
        match pick_files_via_eval(true).await {
            Ok(files) => {
                let mut current = tst.attachments()();
                for (name, data) in files {
                    add_attachment(&mut current, Attachment { name, data });
                }
                tst.attachments().set(current);
            }
            Err(e) => message.set(Some(Msg::Error(e))),
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn open_archive_file_native(tst: Store<TemporaryState>, mut message: Signal<Option<Msg>>, nav: Signal<Nav<Route>>) {
    spawn(async move {
        let files = match pick_files_via_eval(false).await {
            Ok(f) => f,
            Err(e) => {
                message.set(Some(Msg::Error(e)));
                return;
            }
        };
        let (_, bytes) = match files.into_iter().next() {
            Some(f) => f,
            None => return,
        };
        if let Err(e) = open_archive(bytes, tst, nav) {
            message.set(Some(Msg::Error(e)));
        }
    });
}

pub fn build_external(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    atts: &[Attachment],
) -> Result<External, AppError> {
    if atts.is_empty() {
        let note_data = match cipher {
            Some(cipher) => NoteData::CipherText(encrypt_symmetric(note.as_bytes(), password, cipher)?),
            None => NoteData::PlainText(note.to_string()),
        };
        let origin = app_origin().ok_or(AppError::NoNoteInUrl)?;
        let u = build_url(&format!("{}/?screen={}", origin, Screen::Open), &note_data)?;
        match generate_qr_code(&u) {
            Ok(qr) => Ok(External::Note(ExternalNote {
                data: note_data,
                url: u,
                qr,
            })),
            Err(_) => create_archive(note, password, cipher, atts),
        }
    } else {
        create_archive(note, password, cipher, atts)
    }
}

fn create_archive(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    atts: &[Attachment],
) -> Result<External, AppError> {
    let pkg = create_archive_package(note, atts, password, cipher)?;
    Ok(External::Archive(ExternalArchive::new(pkg).infallible()))
}

pub fn generate_share(tst: Store<TemporaryState>) -> Result<(), AppError> {
    tst.external().set(build_external(
        &tst.note()(),
        &tst.password()(),
        tst.cipher()(),
        &tst.attachments()(),
    )?);
    Ok(())
}

fn app_origin() -> Option<String> {
    #[cfg(target_arch = "wasm32")]
    {
        web_sys::window().and_then(|w| {
            let loc = w.location();
            let protocol = loc.protocol().ok()?;
            let host = loc.host().ok()?;
            let pathname = loc.pathname().ok()?;
            let path = pathname.trim_end_matches('/');
            Some(format!("{}//{}{}", protocol, host, path))
        })
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(WEB_APP_URL.to_string())
    }
}

pub fn open_archive(bytes: Vec<u8>, tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> Result<(), AppError> {
    let meta = read_archive_metadata(&bytes)?;
    let screen = match meta.cipher {
        Some(_) => {
            tst.external()
                .set(External::Archive(ExternalArchive::new(bytes).infallible()));
            tst.password().set(String::new());
            Screen::Open
        }
        None => {
            let (text, files) = extract_archive_package(&bytes, "")?;
            tst.note().set(text);
            tst.attachments().set(files);
            tst.external().set(External::Nothing);
            Screen::View
        }
    };
    nav.write().push(screen.to_route(None));
    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
async fn pick_files_via_eval(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, AppError> {
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
    let results = eval
        .recv::<Vec<FileResult>>()
        .await
        .map_err(|e| AppError::FunctoraDioxus(functora_dioxus::Error::JS(e.to_string())))?;
    let mut files = Vec::with_capacity(results.len());
    for f in results {
        match BASE64.decode(&f.data) {
            Ok(bytes) => files.push((f.name, bytes)),
            Err(e) => tracing::warn!("File decode error for {}: {e}", f.name),
        }
    }
    Ok(files)
}

#[cfg(not(target_os = "android"))]
pub fn download_package(data: Vec<u8>, filename: &str) -> Result<String, String> {
    let eval = dioxus::document::eval(&download_script(filename)?);
    let b64 = base64::engine::general_purpose::STANDARD.encode(data);
    eval.send(b64).map_err(|e| e.to_string())?;
    Ok(filename.to_string())
}

pub fn download_script(filename: &str) -> Result<String, String> {
    let name = serde_json::to_string(filename)
        .map_err(|e| e.to_string())?
        .replace('<', "\\u003c")
        .replace('>', "\\u003e")
        .replace('\'', "\\u0027");
    Ok(format!(
        r#"(async function(){{const b64=await dioxus.recv();const bin=atob(b64);const bytes=new Uint8Array(bin.length);for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);const url=URL.createObjectURL(new Blob([bytes],{{type:'application/octet-stream'}}));const a=document.createElement('a');a.href=url;a.download={name};a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>{{document.body.removeChild(a);URL.revokeObjectURL(url)}},1000)}})()"#,
    ))
}

#[cfg(target_os = "android")]
pub fn download_package(data: Vec<u8>, filename: &str) -> Result<String, String> {
    use jni::objects::JObject;
    use jni::JNIEnv;
    use std::sync::mpsc::channel;
    let (tx, rx) = channel();
    let filename = filename.to_string();
    let name = filename.clone();
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
            let jn = env.new_string(&name)?;
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
    rx.recv()
        .map_err(|e| format!("channel error: {e}"))
        .and_then(|r| r.map(|_| filename).map_err(|e| format!("JNI error: {e}")))
}

pub fn add_attachment(current: &mut Vec<Attachment>, att: Attachment) {
    current.retain(|f| f.name != att.name);
    current.push(att);
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
