use crate::crypto::*;
use crate::error::*;
use crate::progress::{Job, Stage};
use crate::worker::Reporter;
use dioxus::prelude::Writable;
use tap::prelude::*;

pub use functora_dioxus::files::Attachment;
pub use functora_dioxus::package::{ArchiveMetadata, ArchiveSource, PackageStages};

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

pub async fn create_zip_async<P>(files: &[Attachment], progress: P) -> Result<Vec<u8>, AppError>
where
    P: Writable<Target = Option<Job>> + 'static,
{
    functora_dioxus::zip::create_zip_async(files, progress, Stage::Zip)
        .await?
        .pipe(Ok)
}

pub(crate) async fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    functora_dioxus::package::package_report(
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

pub async fn create_archive_package_async<P>(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    progress: P,
) -> Result<Vec<u8>, AppError>
where
    P: Writable<Target = Option<Job>> + 'static,
{
    functora_dioxus::package::package_async(
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
    functora_dioxus::package::read_metadata(source)?.pipe(Ok)
}

pub async fn extract_archive_package_async<P>(
    source: ArchiveSource,
    password: &str,
    progress: P,
) -> Result<(String, Vec<Attachment>), AppError>
where
    P: Writable<Target = Option<Job>> + Copy + 'static,
{
    let inner =
        functora_dioxus::package::extract_package_async(source, password, AAD_PREFIX, stages(), progress).await?;
    let mut note = String::new();
    let mut files = Vec::new();
    for (name, data) in functora_dioxus::zip::unzip_async(inner, progress, Stage::Unzip).await? {
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
