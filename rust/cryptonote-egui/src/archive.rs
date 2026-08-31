use crate::crypto::CipherType;
use crate::error::AppError;
use crate::progress::{Job, Stage};
use functora_egui::worker::Reporter;
use tap::prelude::*;

pub use functora_core::package::{ArchiveMetadata, ArchiveSource, PackageStages};
pub use functora_egui::files::Attachment;

const AAD_PREFIX: &[u8] = b"cryptonote.v1";

fn stages() -> PackageStages<Stage> {
    PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt)
}

fn entries_of(note: &str, attachments: &[Attachment]) -> Vec<(String, Vec<u8>)> {
    let mut entries = vec![("note.txt".to_string(), note.as_bytes().to_vec())];
    entries.extend(
        attachments
            .iter()
            .map(|a| (format!("attachments/{}", a.name), a.data.to_vec())),
    );
    entries
}

pub async fn create_zip_async(
    files: &[Attachment],
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<Vec<u8>, AppError> {
    functora_egui::zip::create_zip_async(files, progress, Stage::Zip)
        .await?
        .pipe(Ok)
}

pub(crate) async fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter<Stage>,
) -> Result<Vec<u8>, AppError> {
    functora_core::package::package_report(
        entries_of(note, attachments),
        password,
        cipher,
        AAD_PREFIX,
        stages(),
        report,
    )
    .await?
    .pipe(Ok)
}

pub async fn create_archive_package_async(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<Vec<u8>, AppError> {
    functora_egui::package::package_async(
        entries_of(note, attachments),
        password,
        cipher,
        AAD_PREFIX,
        stages(),
        progress,
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
    progress: impl FnMut(Option<Job<Stage>>) + Send + 'static,
) -> Result<(String, Vec<Attachment>), AppError> {
    use std::sync::{Arc, Mutex};
    let progress_box: Box<dyn FnMut(Option<Job<Stage>>) + Send> = Box::new(progress);
    let progress_arc = Arc::new(Mutex::new(progress_box));
    let progress_clone = Arc::clone(&progress_arc);
    let inner = functora_egui::package::extract_package_async(source, password, AAD_PREFIX, stages(), move |job| {
        if let Ok(mut guard) = progress_arc.lock() {
            guard(job);
        }
    })
    .await?;
    let mut note = String::new();
    let mut files = Vec::new();
    for (name, data) in functora_egui::zip::unzip_async(
        inner,
        move |job| {
            if let Ok(mut guard) = progress_clone.lock() {
                guard(job);
            }
        },
        Stage::Unzip,
    )
    .await?
    {
        if name == "note.txt" {
            note = String::from_utf8(data)?;
        } else {
            files.push(Attachment {
                name: name.strip_prefix("attachments/").unwrap_or(&name).to_string(),
                data: data.into(),
            });
        }
    }
    Ok((note, files))
}

pub async fn extract_archive_package_async_with_progress<F>(
    source: ArchiveSource,
    password: &str,
    progress: F,
) -> Result<(String, Vec<Attachment>), AppError>
where
    F: FnMut(Option<Job<Stage>>) + Send + 'static,
{
    use std::sync::{Arc, Mutex};
    let progress_box: Box<dyn FnMut(Option<Job<Stage>>) + Send> = Box::new(progress);
    let progress_arc = Arc::new(Mutex::new(progress_box));
    let progress_clone = Arc::clone(&progress_arc);
    let inner = functora_egui::package::extract_package_async(source, password, AAD_PREFIX, stages(), move |job| {
        if let Ok(mut guard) = progress_arc.lock() {
            guard(job);
        }
    })
    .await?;
    let mut note = String::new();
    let mut files = Vec::new();
    for (name, data) in functora_egui::zip::unzip_async(
        inner,
        move |job| {
            if let Ok(mut guard) = progress_clone.lock() {
                guard(job);
            }
        },
        Stage::Unzip,
    )
    .await?
    {
        if name == "note.txt" {
            note = String::from_utf8(data)?;
        } else {
            files.push(Attachment {
                name: name.strip_prefix("attachments/").unwrap_or(&name).to_string(),
                data: data.into(),
            });
        }
    }
    Ok((note, files))
}
