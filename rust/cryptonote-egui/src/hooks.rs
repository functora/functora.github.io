use crate::archive::{ArchiveSource, Attachment};
use crate::crypto::CipherType;
use crate::encoding::{NoteData, build_url, decode_note, extract_note_param, generate_qr_code};
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Job, Stage, clear_progress};
use crate::route::Screen;
use crate::state::{External, ExternalNote, TemporaryState};
use crate::storage::APP_ATTRS;
use functora_egui::Routable;
use functora_tagged::InfallibleInto;
use zeroize::Zeroizing;

pub use functora_egui::files::format_size as fmt_size;

#[must_use]
pub fn share_error(cipher: Option<CipherType>, password: &str) -> Option<Msg> {
    (cipher.is_some() && password.is_empty()).then_some(Msg::Base(functora_egui::messages::Msg::PasswordRequired))
}

pub fn reset_temporary_state(state: &mut TemporaryState) {
    state.reset();
}

pub fn add_attachment(current: &mut Vec<Attachment>, att: Attachment) {
    current.retain(|f| f.name != att.name);
    current.push(att);
}

pub fn remove_attachment(state: &mut TemporaryState, index: crate::state::AttachmentIdx) {
    let idx = index.get();
    if idx < state.attachments.len() {
        drop(state.attachments.remove(idx));
    }
}

pub fn extract_note_param_or_err(url: &str) -> Result<String, AppError> {
    extract_note_param(url)
}

pub async fn open_archive_async(source: ArchiveSource, state: &mut TemporaryState) -> Result<Screen, AppError> {
    let meta = crate::archive::read_archive_metadata(&source)?;
    let screen = if meta.cipher.is_some() {
        state.external = External::Archive(crate::crypto::ExternalArchive::new(source.into_bytes()?).infallible());
        state.password.clear();
        clear_progress(&mut state.progress);
        Screen::Open
    } else {
        let (text, files) = crate::archive::extract_archive_package_async(source, "", |_| {}).await?;
        clear_progress(&mut state.progress);
        state.note = text;
        state.attachments = files;
        state.external = External::Nothing;
        Screen::View
    };
    Ok(screen)
}

async fn build_note(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    report: &mut crate::worker::Reporter,
) -> Result<External, AppError> {
    report(Job {
        stage: Stage::Encrypt,
        done: 0,
        total: 1,
        name: None,
    });
    let note_data = match cipher {
        Some(cty) => NoteData::CipherText(crate::crypto::encrypt_symmetric(note.as_bytes(), password, cty)?),
        None => NoteData::PlainText(note.to_string()),
    };
    let base = format!("{}{}", APP_ATTRS.origin(), Screen::Open.to_url());
    let u = build_url(&base, &note_data)?;
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
                .map(|p| External::Archive(crate::crypto::ExternalArchive::new(p).infallible()))
        }
    }
}

pub async fn build_external(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    atts: &[Attachment],
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<External, AppError> {
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
                crate::archive::create_archive_package(
                    &note_owned,
                    &atts_owned,
                    &password_owned,
                    cipher_owned,
                    &mut report,
                )
                .await
                .map(|p| External::Archive(crate::crypto::ExternalArchive::new(p).infallible()))
            }
        },
    )
    .await
}

pub async fn generate_share_async(state: &mut TemporaryState) -> Result<(), AppError> {
    let shared_progress = std::sync::Arc::new(std::sync::Mutex::new(None::<Job<Stage>>));
    let shared_clone = std::sync::Arc::clone(&shared_progress);
    let progress_fn = move |job: Option<Job<Stage>>| {
        if let Ok(mut guard) = shared_progress.lock() {
            *guard = job;
        }
    };
    let external = build_external(
        &state.note.clone(),
        &state.password.clone(),
        state.cipher,
        &state.attachments.clone(),
        progress_fn,
    )
    .await?;
    if let Ok(guard) = shared_clone.lock() {
        state.progress.clone_from(&guard);
    }
    state.external = external;
    clear_progress(&mut state.progress);
    Ok(())
}

pub fn handle_open_url(url: &str, state: &mut TemporaryState) -> Result<Screen, AppError> {
    let note = extract_note_param(url)?;
    match decode_note(&note)? {
        NoteData::CipherText(enc) => {
            state.external = External::Note(ExternalNote {
                data: NoteData::CipherText(enc),
                url: String::new(),
                qr: String::new(),
            });
            Ok(Screen::Open)
        }
        NoteData::PlainText(text) => {
            state.note = text;
            state.cipher = None;
            state.external = External::Nothing;
            Ok(Screen::View)
        }
    }
}
