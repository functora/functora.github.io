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

pub fn share_error(cipher: Option<CipherType>, password: &str) -> Option<Msg> {
    (cipher.is_some() && password.is_empty()).then_some(Msg::Base(BaseMsg::PasswordRequired))
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
    tst.progress().set(None);
}

pub fn reset_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        reset_temporary_state(tst);
        nav.write().push(Screen::Home.to_route(None));
    }
}

pub fn attach_files(tst: Store<TemporaryState>, mut message: Signal<Option<Msg>>) {
    spawn(async move {
        match pick_files_via_eval(true, tst.progress()).await {
            Ok(files) => {
                let mut current = tst.attachments()();
                for (name, data) in files {
                    add_attachment(&mut current, Attachment { name, data });
                }
                tst.attachments().set(current);
                clear_progress(tst.progress());
            }
            Err(e) => {
                message.set(Some(Msg::Error(e)));
                clear_progress(tst.progress());
            }
        }
    });
}

pub fn open_archive_file(tst: Store<TemporaryState>, message: Signal<Option<Msg>>, nav: Signal<Nav<Route>>) {
    spawn(async move {
        let mut message = message;
        let files = match pick_files_via_eval(false, tst.progress()).await {
            Ok(f) => f,
            Err(e) => {
                tst.progress().set(None);
                message.set(Some(Msg::Error(e)));
                return;
            }
        };
        let (_, bytes) = match files.into_iter().next() {
            Some(f) => f,
            None => {
                tst.progress().set(None);
                return;
            }
        };
        if let Err(e) = open_archive_async(ArchiveSource::Bytes(bytes), tst, nav).await {
            message.set(Some(Msg::Error(e)));
        }
    });
}

pub async fn open_archive_async(
    source: ArchiveSource,
    tst: Store<TemporaryState>,
    mut nav: Signal<Nav<Route>>,
) -> Result<(), AppError> {
    let meta = read_archive_metadata(&source)?;
    let screen = match meta.cipher {
        Some(_) => {
            tst.external().set(External::Archive(
                ExternalArchive::new(source.into_bytes()?).infallible(),
            ));
            tst.password().set(String::new());
            clear_progress(tst.progress());
            Screen::Open
        }
        None => {
            let (text, files) = extract_archive_package_async(source, "", tst.progress()).await?;
            clear_progress(tst.progress());
            tst.note().set(text);
            tst.attachments().set(files);
            tst.external().set(External::Nothing);
            Screen::View
        }
    };
    nav.write().push(screen.to_route(None));
    Ok(())
}

async fn build_note(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter,
) -> Result<External, AppError> {
    report(Job {
        stage: Stage::Encrypt,
        done: 0,
        total: 1,
        name: None,
    });
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
        Err(_) => crate::archive::create_archive_package(note, &[], password, cipher, report)
            .await
            .map(|p| External::Archive(ExternalArchive::new(p).infallible())),
    }
}

pub async fn build_external<P>(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    atts: &[Attachment],
    progress: P,
) -> Result<External, AppError>
where
    P: Writable<Target = Option<Job>> + 'static,
{
    let note = note.to_string();
    let password = password.to_string();
    let atts = atts.to_vec();
    crate::worker::run(
        (note, password, cipher, atts),
        progress,
        |(note, password, cipher, atts), mut report| async move {
            if atts.is_empty() {
                build_note(&note, &password, cipher, &mut report).await
            } else {
                create_archive_package(&note, &atts, &password, cipher, &mut report)
                    .await
                    .map(|p| External::Archive(ExternalArchive::new(p).infallible()))
            }
        },
    )
    .await
}

pub async fn generate_share_async(tst: Store<TemporaryState>) -> Result<(), AppError> {
    let external = build_external(
        &tst.note()(),
        &tst.password()(),
        tst.cipher()(),
        &tst.attachments()(),
        tst.progress(),
    )
    .await?;
    tst.external().set(external);
    clear_progress(tst.progress());
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

pub fn pick_script(multiple: bool) -> String {
    format!(
        r#"
        (async function() {{
            const CHUNK = 2 * 1024 * 1024;
            const input = document.createElement('input');
            input.type = 'file';
            input.multiple = {multiple};
            input.style.display = 'none';
            document.body.appendChild(input);
            const toBase64 = (uint8) => {{
                let bin = '';
                for (let i = 0; i < uint8.length; i += 0x8000) {{
                    bin += String.fromCharCode.apply(null, uint8.subarray(i, i + 0x8000));
                }}
                return btoa(bin);
            }};
            const done = () => dioxus.send({{ t: 'done' }});
            try {{
                const files = await new Promise((resolve) => {{
                    const timer = setTimeout(() => {{ input.remove(); resolve([]); }}, 120000);
                    input.addEventListener('change', () => {{
                        clearTimeout(timer);
                        resolve(Array.from(input.files));
                    }}, {{ once: true }});
                    input.click();
                }});
                for (const f of files) {{
                    dioxus.send({{ t: 'begin', name: f.name, size: f.size }});
                    for (let off = 0; off < f.size; off += CHUNK) {{
                        const buf = await f.slice(off, Math.min(off + CHUNK, f.size)).arrayBuffer();
                        dioxus.send({{ t: 'chunk', data: toBase64(new Uint8Array(buf)) }});
                    }}
                }}
                input.remove();
                done();
            }} catch(e) {{
                input.remove();
                done();
            }}
        }})()
        "#,
        multiple = if multiple { "true" } else { "false" }
    )
}

async fn pick_files_via_eval<P>(multiple: bool, progress: P) -> Result<Vec<(String, Vec<u8>)>, AppError>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
{
    use base64::engine::general_purpose::STANDARD as BASE64;

    #[derive(Deserialize)]
    #[serde(tag = "t", rename_all = "lowercase")]
    enum PickMsg {
        Begin { name: String, size: u64 },
        Chunk { data: String },
        Done,
    }

    let mut eval = dioxus::document::eval(&pick_script(multiple));
    let mut files: Vec<(String, Vec<u8>)> = Vec::new();
    let mut done = 0u64;
    let mut total = 0u64;
    loop {
        let msg = eval
            .recv::<PickMsg>()
            .await
            .map_err(|e| AppError::FunctoraDioxus(functora_dioxus::Error::JS(e.to_string())))?;
        match msg {
            PickMsg::Begin { name, size } => {
                total += size;
                files.push((name, Vec::new()));
                done = files.iter().map(|(_, b)| b.len() as u64).sum();
            }
            PickMsg::Chunk { data } => {
                if let Some((_, buf)) = files.last_mut() {
                    match BASE64.decode(&data) {
                        Ok(bytes) => {
                            done += bytes.len() as u64;
                            buf.extend(bytes);
                        }
                        Err(e) => {
                            tracing::warn!("File decode error: {e}");
                            files.pop();
                        }
                    }
                }
            }
            PickMsg::Done => break,
        }
        match files.last() {
            Some((name, _)) => report_progress_named(progress, Stage::Attach, done, total, name).await,
            None => report_progress(progress, Stage::Attach, done, total).await,
        }
    }
    Ok(files)
}

#[cfg(not(target_os = "android"))]
#[derive(Serialize)]
struct DownloadMsg {
    t: &'static str,
    data: String,
}

#[cfg(not(target_os = "android"))]
pub async fn download_package<P>(data: Vec<u8>, filename: &str, progress: P) -> Result<String, String>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
{
    use base64::engine::general_purpose::STANDARD as BASE64;
    const SEND_CHUNK: usize = 3 * 1024 * 1024;
    let eval = dioxus::document::eval(&download_script(filename)?);
    let total = data.len() as u64;
    let mut done = 0u64;
    for chunk in data.chunks(SEND_CHUNK) {
        eval.send(DownloadMsg {
            t: "chunk",
            data: BASE64.encode(chunk),
        })
        .map_err(|e| e.to_string())?;
        done += chunk.len() as u64;
        report_progress(progress, Stage::Download, done, total).await;
    }
    eval.send(DownloadMsg {
        t: "done",
        data: String::new(),
    })
    .map_err(|e| e.to_string())?;
    Ok(filename.to_string())
}

pub fn download_script(filename: &str) -> Result<String, String> {
    let name = serde_json::to_string(filename)
        .map_err(|e| e.to_string())?
        .replace('<', "\\u003c")
        .replace('>', "\\u003e")
        .replace('\'', "\\u0027");
    Ok(format!(
        r#"(async function(){{const parts=[];for(;;){{const m=await dioxus.recv();if(m&&m.t==='done')break;const bin=atob(m.data);const bytes=new Uint8Array(bin.length);for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);parts.push(bytes)}}const url=URL.createObjectURL(new Blob(parts,{{type:'application/octet-stream'}}));const a=document.createElement('a');a.href=url;a.download={name};a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>{{document.body.removeChild(a);URL.revokeObjectURL(url)}},1000)}})()"#,
    ))
}

#[cfg(target_os = "android")]
pub async fn download_package<P>(data: Vec<u8>, filename: &str, progress: P) -> Result<String, String>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
{
    let filename = filename.to_string();
    let name = filename.clone();
    crate::worker::run(
        (data, name, filename),
        progress,
        |(data, name, filename), mut report| async move {
            download_android(&data, name, filename, &mut report)
                .await
                .map_err(|e| AppError::FunctoraDioxus(functora_dioxus::Error::JNI(e)))
        },
    )
    .await
    .map_err(|e| e.to_string())
}

#[cfg(target_os = "android")]
async fn download_android(
    data: &[u8],
    name: String,
    filename: String,
    report: &mut Reporter,
) -> Result<String, String> {
    use jni::objects::{GlobalRef, JObject};
    use jni::JNIEnv;
    use std::sync::mpsc::channel;
    use std::sync::{Arc, Mutex};
    const WRITE_CHUNK: usize = 4 * 1024 * 1024;
    let total = data.len() as u64;
    report(Job {
        stage: Stage::Download,
        done: 0,
        total,
        name: None,
    });
    let stream: Arc<Mutex<Option<GlobalRef>>> = Arc::new(Mutex::new(None));
    let (tx, rx) = channel();
    let open = stream.clone();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, activity: &JObject, _| {
        let res = (|| -> Result<(), jni::errors::Error> {
            let os = open_stream(env, activity, &name)?;
            let os = env.new_global_ref(os)?;
            if let Ok(mut slot) = open.lock() {
                *slot = Some(os);
            }
            Ok(())
        })();
        if let Err(ref error) = res {
            tracing::error!("MediaStore JNI open error: {error}");
            let _ = env.exception_describe();
        }
        let _ = env.exception_clear();
        let _ = tx.send(res);
    });
    rx.recv()
        .map_err(|e| format!("channel error: {e}"))?
        .map_err(|e| format!("JNI open error: {e}"))?;
    let mut done = 0u64;
    for chunk in data.chunks(WRITE_CHUNK) {
        let (tx, rx) = channel();
        let stream = stream.clone();
        let size = chunk.len() as u64;
        let chunk = chunk.to_vec();
        dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, _, _| {
            let res = (|| -> Result<(), jni::errors::Error> {
                let guard = stream.lock().map_err(|_| jni::errors::Error::JavaException)?;
                let os = guard.as_ref().cloned().ok_or(jni::errors::Error::JavaException)?;
                let ba = env.byte_array_from_slice(&chunk)?;
                env.call_method(os.as_obj(), "write", "([B)V", &[(&ba).into()])?;
                Ok(())
            })();
            if let Err(ref error) = res {
                tracing::error!("MediaStore JNI write error: {error}");
                let _ = env.exception_describe();
            }
            let _ = env.exception_clear();
            let _ = tx.send(res);
        });
        rx.recv()
            .map_err(|e| format!("channel error: {e}"))?
            .map_err(|e| format!("JNI write error: {e}"))?;
        done += size;
        report(Job {
            stage: Stage::Download,
            done,
            total,
            name: None,
        });
    }
    let (tx, rx) = channel();
    let stream = stream.clone();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut JNIEnv, _, _| {
        let res = (|| -> Result<(), jni::errors::Error> {
            if let Some(os) = stream.lock().map_err(|_| jni::errors::Error::JavaException)?.take() {
                env.call_method(os.as_obj(), "close", "()V", &[])?;
            }
            Ok(())
        })();
        if let Err(ref error) = res {
            tracing::error!("MediaStore JNI close error: {error}");
            let _ = env.exception_describe();
        }
        let _ = env.exception_clear();
        let _ = tx.send(res);
    });
    rx.recv()
        .map_err(|e| format!("channel error: {e}"))
        .and_then(|r| r.map(|_| filename.to_string()).map_err(|e| format!("JNI error: {e}")))
}

#[cfg(target_os = "android")]
fn open_stream<'local>(
    env: &mut jni::JNIEnv<'local>,
    activity: &jni::objects::JObject,
    name: &str,
) -> Result<jni::objects::JObject<'local>, jni::errors::Error> {
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
    env.call_method(
        &cv,
        "put",
        "(Ljava/lang/String;Ljava/lang/String;)V",
        &[(&nk).into(), (&jn).into()],
    )?;
    let mk = env.new_string("mime_type")?;
    let mv = mime_for(env, name)?;
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
    env.call_method(
        &resolver,
        "openOutputStream",
        "(Landroid/net/Uri;)Ljava/io/OutputStream;",
        &[(&uri).into()],
    )?
    .l()
}

#[cfg(target_os = "android")]
fn mime_for<'local>(
    env: &mut jni::JNIEnv<'local>,
    name: &str,
) -> Result<jni::objects::JString<'local>, jni::errors::Error> {
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
