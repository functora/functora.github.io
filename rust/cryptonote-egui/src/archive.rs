use crate::crypto::CipherType;
use crate::error::AppError;
use crate::task::Reporter;
use functora_core::files::Attachment;
use functora_core::package::{
    extract_package_report, package_report, read_metadata, PackageStages,
};
pub use functora_core::package::{ArchiveMetadata, ArchiveSource};
use functora_core::progress::Stage;
use tap::prelude::*;

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

pub async fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    package_report(
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

pub fn read_archive_metadata(source: &ArchiveSource) -> Result<ArchiveMetadata, AppError> {
    read_metadata(source)?.pipe(Ok)
}

pub async fn extract_archive_package(
    source: ArchiveSource,
    password: &str,
    report: &mut Reporter,
) -> Result<(String, Vec<Attachment>), AppError> {
    let inner = extract_package_report(source, password, AAD_PREFIX, stages(), report).await?;
    let mut note = String::new();
    let mut files = Vec::new();
    for (name, data) in functora_core::zip::unzip_report(inner, Stage::Unzip, report).await? {
        if name == "note.txt" {
            note = String::from_utf8(data)?;
        } else {
            files.push(Attachment {
                name: name
                    .strip_prefix("attachments/")
                    .unwrap_or(&name)
                    .to_string(),
                data: data.into(),
            });
        }
    }
    Ok((note, files))
}
