use crate::error::AppError;
use crate::messages::Msg;
use crate::*;
use zeroize::Zeroizing;

pub use functora_dioxus::files::format_size;
pub use functora_dioxus::hooks::{use_lang, use_message_markdown};

#[must_use]
pub fn use_message() -> Signal<Option<Msg>> {
    functora_dioxus::hooks::use_message()
}

pub fn read_clipboard(on_paste: impl FnOnce(String) + 'static, mut message: Signal<Option<Msg>>) {
    let _ = spawn(async move {
        match functora_dioxus::ffi::read_clipboard().await {
            Ok(text) => on_paste(text),
            Err(e) => message.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into()))),
        }
    });
}

pub fn write_clipboard(val: String, message: Signal<Option<Msg>>) {
    functora_dioxus::ffi::write_clipboard(val, message, Msg::Base(BaseMsg::Copied), |e| {
        Msg::Base(BaseMsg::ClipboardWriteError(e.to_string()))
    });
}

#[must_use]
pub fn share_error(cipher: Option<CipherType>, password: &str) -> Option<Msg> {
    (cipher.is_some() && password.is_empty()).then_some(Msg::Base(BaseMsg::PasswordRequired))
}

pub fn edit_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        tst.action().set(ActionMode::Create);
        nav.write().push(Screen::Home.to_route(None));
    }
}

/// IMPORTANT! Do not remove the per-field resets below: keep them in sync with
/// new `TemporaryState` fields. `tst.set(TemporaryState::default())` is kept as a safety
/// net, but dioxus-stores 0.7.2 fails to notify field subscribers on whole-store
/// writes (`paths_under` bug, fixed in 0.7.4 via <https://github.com/DioxusLabs/dioxus/pull/5069>),
/// so the per-field writes mark every field dirty.
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
    tst.attachment().set(None);
}

pub fn reset_handler(tst: Store<TemporaryState>, mut nav: Signal<Nav<Route>>) -> impl FnMut(MouseEvent) + 'static {
    move |_| {
        reset_temporary_state(tst);
        nav.write().push(Screen::Home.to_route(None));
    }
}

pub fn attach_files(tst: Store<TemporaryState>, mut message: Signal<Option<Msg>>) {
    let _ = spawn(async move {
        match functora_dioxus::files::pick_files(true, tst.progress(), Stage::Attach).await {
            Ok(files) => {
                let next = files
                    .into_iter()
                    .fold(tst.attachments()(), |mut current, (name, data)| {
                        add_attachment(
                            &mut current,
                            Attachment {
                                name,
                                data: data.into(),
                            },
                        );
                        current
                    });
                tst.attachments().set(next);
                clear_progress(tst.progress());
            }
            Err(e) => {
                message.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into())));
                clear_progress(tst.progress());
            }
        }
    });
}

pub fn open_archive_file(tst: Store<TemporaryState>, message: Signal<Option<Msg>>, nav: Signal<Nav<Route>>) {
    let _ = spawn(async move {
        let mut message_out = message;
        let files = match functora_dioxus::files::pick_files(false, tst.progress(), Stage::Attach).await {
            Ok(f) => f,
            Err(e) => {
                tst.progress().set(None);
                message_out.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into())));
                return;
            }
        };
        let Some((_, bytes)) = files.into_iter().next() else {
            tst.progress().set(None);
            return;
        };
        if let Err(e) = open_archive_async(ArchiveSource::Bytes(bytes), tst, nav).await {
            message_out.set(Some(Msg::Error(e.into())));
        }
    });
}

pub async fn open_archive_async(
    source: ArchiveSource,
    tst: Store<TemporaryState>,
    mut nav: Signal<Nav<Route>>,
) -> Result<(), AppError> {
    let meta = read_archive_metadata(&source)?;
    let screen = if meta.cipher.is_some() {
        tst.external().set(External::Archive(
            ExternalArchive::new(source.into_bytes()?).infallible(),
        ));
        tst.password().set(String::new());
        clear_progress(tst.progress());
        Screen::Open
    } else {
        let (text, files) = extract_archive_package_async(source, "", tst.progress()).await?;
        clear_progress(tst.progress());
        tst.note().set(text);
        tst.attachments().set(files);
        tst.external().set(External::Nothing);
        Screen::View
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
        Some(cty) => NoteData::CipherText(encrypt_symmetric(note.as_bytes(), password, cty)?),
        None => NoteData::PlainText(note.to_string()),
    };
    #[cfg(target_arch = "wasm32")]
    let origin = app_origin().ok_or(AppError::NoNoteInUrl)?;
    #[cfg(not(target_arch = "wasm32"))]
    let origin = app_origin();
    let u = build_url(&format!("{}/?screen={}", origin, Screen::Open), &note_data)?;
    match generate_qr_code(&u) {
        Ok(qr) => Ok(External::Note(ExternalNote {
            data: note_data,
            url: u,
            qr,
        })),
        Err(e) => {
            tracing::warn!("QR code generation failed: {e}");
            crate::archive::create_archive_package(note, &[], password, cipher, report)
                .await
                .map(|p| External::Archive(ExternalArchive::new(p).infallible()))
        }
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
    crate::worker::run(
        (
            note.to_string(),
            Zeroizing::new(password.to_string()),
            cipher,
            atts.to_vec(),
        ),
        progress,
        |(note_owned, password_owned, cipher_owned, atts_owned), mut report| async move {
            if atts_owned.is_empty() {
                build_note(&note_owned, &password_owned, cipher_owned, &mut report).await
            } else {
                create_archive_package(&note_owned, &atts_owned, &password_owned, cipher_owned, &mut report)
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

#[cfg(target_arch = "wasm32")]
fn app_origin() -> Option<String> {
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
fn app_origin() -> String {
    APP_ATTRS.app_url()
}

#[cfg(not(target_os = "android"))]
pub async fn download_package<P, D>(data: D, filename: &str, progress: P) -> Result<String, functora_dioxus::Error>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
    D: AsRef<[u8]>,
{
    functora_dioxus::files::download_package(data, filename, progress, Stage::Download).await
}

#[cfg(target_os = "android")]
pub async fn download_package<P, D>(data: D, filename: &str, progress: P) -> Result<String, functora_dioxus::Error>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
    D: AsRef<[u8]> + Send + 'static,
{
    let name = filename.to_string();
    crate::worker::run((data, name), progress, |(bytes, file_name), mut report| async move {
        functora_dioxus::android::save_to_downloads(bytes.as_ref(), file_name.clone(), move |done, total| {
            report(Job {
                stage: Stage::Download,
                done,
                total,
                name: None,
            });
        })?;
        Ok(file_name)
    })
    .await
}

pub fn add_attachment(current: &mut Vec<Attachment>, att: Attachment) {
    current.retain(|f| f.name != att.name);
    current.push(att);
}

pub fn remove_attachment(tst: Store<TemporaryState>, index: usize) {
    let mut atts = tst.attachments()();
    let _ = atts.remove(index);
    tst.attachments().set(atts);
}

pub fn download_attachment<P>(att: Attachment, progress: P, mut message: Signal<Option<Msg>>)
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
{
    let _ = spawn(async move {
        let mut progress_out = progress;
        match download_package(att.data, &att.name, progress_out).await {
            Ok(loc) => {
                progress_out.set(None);
                message.set(Some(Msg::Downloaded(loc)));
            }
            Err(e) => {
                progress_out.set(None);
                message.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into())));
            }
        }
    });
}
