use crate::archive::{create_archive_package, extract_archive_package};
use crate::crypto::{CipherType, decrypt_symmetric, encrypt_symmetric};
use crate::encoding::{NoteData, build_url};
use crate::error::{AppError, MsgError};
use crate::messages::Msg;
use crate::progress::{Job, Stage};
use crate::state::{External, ExternalArchive, ExternalNote};
use functora_core::files::Attachment;
use functora_core::package::ArchiveSource;
use functora_core::worker::Reporter as CoreReporter;
use functora_tagged::InfallibleInto;
use std::sync::mpsc::Sender;
use zeroize::Zeroizing;

pub type Reporter = CoreReporter<Stage>;

pub enum Event {
    Job(Option<Job>),
    Message(Msg),
    ExternalReady(Result<External, MsgError>),
    Opened(Result<(String, Vec<Attachment>), MsgError>),
    Clipboard(Result<String, AppError>),
    Picked(Result<Vec<(String, Vec<u8>)>, AppError>),
    Scanned(Result<String, AppError>),
    Downloaded(Result<Option<String>, AppError>),
}

pub fn send(tx: &Sender<Event>, ctx: &egui::Context, event: Event) {
    if tx.send(event).is_ok() {
        ctx.request_repaint();
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn_async(fut: impl std::future::Future<Output = ()> + Send + 'static) {
    _ = std::thread::Builder::new()
        .name("cryptonote-worker".into())
        .spawn(move || {
            futures_executor::block_on(fut);
        });
}

#[cfg(target_arch = "wasm32")]
pub fn spawn_async(fut: impl std::future::Future<Output = ()> + 'static) {
    wasm_bindgen_futures::spawn_local(fut);
}

pub fn build_external(
    note: String,
    password: String,
    cipher: Option<CipherType>,
    attachments: Vec<Attachment>,
    tx: Sender<Event>,
    ctx: egui::Context,
    origin: String,
) {
    spawn_async(async move {
        let result = functora_core::worker::run(
            (note, password, cipher, attachments),
            {
                let job_tx = tx.clone();
                let job_ctx = ctx.clone();
                move |job| send(&job_tx, &job_ctx, Event::Job(job))
            },
            |(note_in, password_in, cipher_in, atts_in), mut report| async move {
                if atts_in.is_empty() {
                    build_note(&note_in, &password_in, cipher_in, &origin, &mut report).await
                } else {
                    create_archive_package(&note_in, &atts_in, &password_in, cipher_in, &mut report)
                        .await
                        .map(|bytes| External::Archive(ExternalArchive::new(bytes).infallible()))
                }
            },
        )
        .await;
        send(
            &tx,
            &ctx,
            Event::ExternalReady(result.map_err(MsgError::from)),
        );
    });
}

async fn build_note(
    note: &str,
    password: &str,
    cipher: Option<CipherType>,
    origin: &str,
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
    let u = build_url(&format!("{origin}/?screen=open"), &note_data)?;
    match functora_core::encoding::generate_qr_code(&u) {
        Ok(qr) => Ok(External::Note(ExternalNote {
            data: note_data,
            url: u,
            qr,
        })),
        Err(e) => {
            log::warn!("QR code generation failed: {e}");
            create_archive_package(note, &[], password, cipher, report)
                .await
                .map(|bytes| External::Archive(ExternalArchive::new(bytes).infallible()))
        }
    }
}

pub fn decrypt_external(
    encrypted: functora_core::crypto::EncryptedNote,
    password: String,
    tx: Sender<Event>,
    ctx: egui::Context,
) {
    spawn_async(async move {
        let result = functora_core::worker::run(
            (encrypted, Zeroizing::new(password)),
            {
                let job_tx = tx.clone();
                let job_ctx = ctx.clone();
                move |job| send(&job_tx, &job_ctx, Event::Job(job))
            },
            |(enc, pwd), mut report| async move {
                report(Job {
                    stage: Stage::Decrypt,
                    done: 0,
                    total: 1,
                    name: None,
                });
                let plaintext = decrypt_symmetric(&enc, &pwd)?;
                String::from_utf8(plaintext)
                    .map(|text| (text, Vec::new()))
                    .map_err(AppError::from)
            },
        )
        .await;
        send(&tx, &ctx, Event::Opened(result.map_err(MsgError::from)));
    });
}

pub fn extract_archive(bytes: Vec<u8>, password: String, tx: Sender<Event>, ctx: egui::Context) {
    spawn_async(async move {
        let result = functora_core::worker::run(
            (bytes, Zeroizing::new(password)),
            {
                let job_tx = tx.clone();
                let job_ctx = ctx.clone();
                move |job| send(&job_tx, &job_ctx, Event::Job(job))
            },
            |(bytes_in, pwd), mut report| async move {
                extract_archive_package(ArchiveSource::Bytes(bytes_in), &pwd, &mut report).await
            },
        )
        .await;
        send(&tx, &ctx, Event::Opened(result.map_err(MsgError::from)));
    });
}
