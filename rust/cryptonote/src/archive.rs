use crate::crypto::{AAD_PREFIX, CipherType};
use crate::error::AppError;
use crate::progress::{Job, Stage};
use functora_egui::worker::Reporter;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};
use tap::prelude::*;

pub use functora_core::package::{ArchiveMetadata, ArchiveSource, PackageStages};
pub use functora_core::zip::ATTACHMENTS_PREFIX;
pub use functora_egui::files::Attachment;

pub const NOTE_ENTRY: &str = "note.txt";

/// Reduce an untrusted entry name to a safe, single-component file name.
/// Archive entries and picked file names may carry directory separators or
/// `..` components; keeping only the last path component and dropping
/// leading dots makes the name safe to show and to write to disk.
#[must_use]
pub fn sanitise_attachment_name(name: &str) -> String {
    let base = name
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or(name)
        .trim()
        .trim_start_matches('.')
        .trim();
    let cleaned: String = base.chars().filter(|c| !c.is_control()).collect();
    if cleaned.is_empty() {
        "attachment".to_string()
    } else {
        cleaned
    }
}

fn stages() -> PackageStages<Stage> {
    PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt)
}

fn entries_of(note: &str, attachments: &[Attachment]) -> Vec<(String, Arc<[u8]>)> {
    let mut entries = vec![(NOTE_ENTRY.to_string(), Arc::from(note.as_bytes().to_vec()))];
    entries.extend(attachments.iter().map(|a| {
        (
            format!("{ATTACHMENTS_PREFIX}{}", sanitise_attachment_name(&a.name)),
            Arc::clone(&a.data),
        )
    }));
    entries
}

pub(crate) async fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    cancel: Arc<AtomicBool>,
    report: &mut Reporter<Stage>,
) -> Result<Vec<u8>, AppError> {
    functora_core::package::package_report(
        entries_of(note, attachments),
        password,
        cipher,
        AAD_PREFIX,
        stages(),
        report,
        Some(&cancel),
    )
    .await?
    .pipe(Ok)
}

pub async fn create_archive_package_async(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    cancel: Arc<AtomicBool>,
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<Vec<u8>, AppError> {
    functora_egui::package::package_async(
        entries_of(note, attachments),
        password,
        cipher,
        AAD_PREFIX,
        stages(),
        progress,
        cancel,
    )
    .await?
    .pipe(Ok)
}

pub fn read_archive_metadata(source: &ArchiveSource) -> Result<ArchiveMetadata, AppError> {
    functora_core::package::read_metadata(source)?.pipe(Ok)
}

pub async fn extract_archive_package_async(
    source: ArchiveSource,
    password: &str,
    cancel: Arc<AtomicBool>,
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<(String, Vec<Attachment>), AppError> {
    extract_archive_package_async_with_progress(source, password, cancel, progress).await
}

pub async fn extract_archive_package_async_with_progress<F>(
    source: ArchiveSource,
    password: &str,
    cancel: Arc<AtomicBool>,
    progress: F,
) -> Result<(String, Vec<Attachment>), AppError>
where
    F: FnMut(Option<Job<Stage>>) + Send + 'static,
{
    let progress_box: Box<dyn FnMut(Option<Job<Stage>>) + Send> = Box::new(progress);
    let progress_arc = Arc::new(Mutex::new(progress_box));
    let progress_clone = Arc::clone(&progress_arc);
    let inner = functora_egui::package::extract_package_async(
        source,
        password,
        AAD_PREFIX,
        stages(),
        move |job| {
            let mut guard = progress_arc.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
            guard(job);
        },
        Arc::clone(&cancel),
    )
    .await?;
    let mut note = String::new();
    let mut files = Vec::new();
    for (name, data) in functora_egui::zip::unzip_async(
        inner,
        move |job| {
            let mut guard = progress_clone.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
            guard(job);
        },
        Stage::Unzip,
        cancel,
    )
    .await?
    {
        if name == NOTE_ENTRY {
            note = String::from_utf8(data)?;
        } else {
            files.push(Attachment {
                name: sanitise_attachment_name(name.strip_prefix(ATTACHMENTS_PREFIX).unwrap_or(&name)),
                data: data.into(),
            });
        }
    }
    Ok((note, files))
}
