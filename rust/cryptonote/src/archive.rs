use crate::crypto::*;
use crate::error::*;
use crate::progress::{yield_to_paint, Job, Stage};
use crate::worker;
use crate::worker::Reporter;
use dioxus::prelude::Writable;
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use tap::prelude::*;
use zip::CompressionMethod;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArchiveMetadata {
    pub cipher: Option<CipherType>,
    pub kdf: Kdf,
    pub nonce: Vec<u8>,
    pub salt: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attachment {
    pub name: String,
    pub data: Vec<u8>,
}

const METADATA_ENTRY: &str = "metadata.json";
const PAYLOAD_ENTRY: &str = "payload.cpt";
const ZIP_CHUNK: usize = 2 * 1024 * 1024;

fn opts() -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default()
        .compression_method(CompressionMethod::Deflated)
        .compression_level(Some(1))
}

async fn write_entry(
    zip: &mut zip::ZipWriter<std::io::Cursor<&mut Vec<u8>>>,
    name: &str,
    data: &[u8],
    report: &mut Reporter,
    stage: Stage,
    done: &mut u64,
    total: u64,
) -> Result<(), AppError> {
    zip.start_file(name, opts())
        .map_err(|e| AppError::Archive(e.to_string()))?;
    let display = name.strip_prefix("attachments/").unwrap_or(name);
    for chunk in data.chunks(ZIP_CHUNK) {
        zip.write_all(chunk).map_err(|e| AppError::Archive(e.to_string()))?;
        *done += chunk.len() as u64;
        report(Job {
            stage,
            done: *done,
            total,
            name: Some(display.to_string()),
        });
        yield_to_paint().await;
    }
    Ok(())
}

pub async fn create_zip_async<P>(files: &[Attachment], progress: P) -> Result<Vec<u8>, AppError>
where
    P: Writable<Target = Option<Job>> + 'static,
{
    let files = files.to_vec();
    worker::run(files, progress, |files, mut report| async move {
        let mut buf = Vec::new();
        let total = files.iter().map(|f| f.data.len() as u64).sum::<u64>();
        let mut done = 0u64;
        {
            let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut buf));
            for f in &files {
                write_entry(&mut zip, &f.name, &f.data, &mut report, Stage::Zip, &mut done, total).await?;
            }
            zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
        }
        Ok(buf)
    })
    .await
}

pub(crate) async fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    let total = note.len() as u64 + attachments.iter().map(|a| a.data.len() as u64).sum::<u64>();
    let mut inner = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut inner));
        let mut done = 0u64;
        write_entry(
            &mut zip,
            "note.txt",
            note.as_bytes(),
            report,
            Stage::Zip,
            &mut done,
            total,
        )
        .await?;
        for att in attachments {
            write_entry(
                &mut zip,
                &format!("attachments/{}", att.name),
                &att.data,
                report,
                Stage::Zip,
                &mut done,
                total,
            )
            .await?;
        }
        zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
    }
    package(&inner, password, cipher, report).await
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
    let note = note.to_string();
    let attachments = attachments.to_vec();
    let password = password.to_string();
    worker::run(
        (note, attachments, password, cipher),
        progress,
        |(note, attachments, password, cipher), mut report| async move {
            create_archive_package(&note, &attachments, &password, cipher, &mut report).await
        },
    )
    .await
}

fn write_package_entries(
    zip: &mut zip::ZipWriter<std::io::Cursor<&mut Vec<u8>>>,
    meta: &ArchiveMetadata,
) -> Result<(), AppError> {
    let stored: zip::write::FileOptions<'static, ()> =
        zip::write::FileOptions::default().compression_method(CompressionMethod::Stored);
    zip.start_file(METADATA_ENTRY, stored)
        .map_err(|e| AppError::Archive(e.to_string()))?;
    zip.write_all(&serde_json::to_vec(meta)?)
        .map_err(|e| AppError::Archive(e.to_string()))?;
    zip.start_file(PAYLOAD_ENTRY, stored)
        .map_err(|e| AppError::Archive(e.to_string()))?;
    Ok(())
}

async fn package_with_encryption(
    inner: &[u8],
    password: &str,
    cipher: CipherType,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    let parts = StreamParts::derive(password, cipher)?;
    let meta = ArchiveMetadata {
        cipher: Some(cipher),
        kdf: Kdf::Argon2id,
        nonce: parts.nonce().to_vec(),
        salt: parts.salt().to_vec(),
    };
    let mut pkg = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut pkg));
        write_package_entries(&mut zip, &meta)?;
        let mut offset = 0usize;
        let mut position = 0u32;
        while offset < inner.len() {
            let end = (offset + STREAM_CHUNK).min(inner.len());
            let last = end == inner.len();
            let ct = parts.encrypt_chunk(position, last, &inner[offset..end])?;
            zip.write_all(&ct).map_err(|e| AppError::Archive(e.to_string()))?;
            offset = end;
            position += 1;
            report(Job {
                stage: Stage::Encrypt,
                done: offset as u64,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
    }
    Ok(pkg)
}

async fn package_plain(inner: &[u8], report: &mut Reporter) -> Result<Vec<u8>, AppError> {
    let meta = ArchiveMetadata {
        cipher: None,
        kdf: Kdf::Argon2id,
        nonce: Vec::new(),
        salt: Vec::new(),
    };
    let mut pkg = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut pkg));
        write_package_entries(&mut zip, &meta)?;
        let mut done = 0u64;
        for chunk in inner.chunks(ZIP_CHUNK) {
            zip.write_all(chunk).map_err(|e| AppError::Archive(e.to_string()))?;
            done += chunk.len() as u64;
            report(Job {
                stage: Stage::Zip,
                done,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
    }
    Ok(pkg)
}

async fn package(
    inner: &[u8],
    password: &str,
    cipher: Option<CipherType>,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    match cipher {
        Some(cipher) => package_with_encryption(inner, password, cipher, report).await,
        None => package_plain(inner, report).await,
    }
}

pub fn read_archive_metadata(package: &[u8]) -> Result<ArchiveMetadata, AppError> {
    let mut archive =
        zip::ZipArchive::new(std::io::Cursor::new(package)).map_err(|e| AppError::Archive(e.to_string()))?;
    let mut meta_file = archive
        .by_name(METADATA_ENTRY)
        .map_err(|e| AppError::Archive(e.to_string()))?;
    let mut meta_json = Vec::new();
    meta_file
        .read_to_end(&mut meta_json)
        .map_err(|e| AppError::Archive(e.to_string()))?;
    serde_json::from_slice::<ArchiveMetadata>(&meta_json)?.pipe(Ok)
}

pub async fn extract_archive_package_async<P>(
    package: &[u8],
    password: &str,
    progress: P,
) -> Result<(String, Vec<Attachment>), AppError>
where
    P: Writable<Target = Option<Job>> + 'static,
{
    let package = package.to_vec();
    let password = password.to_string();
    worker::run(
        (package, password),
        progress,
        |(package, password), mut report| async move {
            let (meta, payload) = read_package(&package)?;
            let inner = match meta.cipher {
                Some(cipher) => unseal(&meta, &payload, &password, cipher, &mut report).await?,
                None => payload,
            };
            unzip_inner(inner, &mut report).await
        },
    )
    .await
}

fn read_package(package: &[u8]) -> Result<(ArchiveMetadata, Vec<u8>), AppError> {
    let mut archive =
        zip::ZipArchive::new(std::io::Cursor::new(package)).map_err(|e| AppError::Archive(e.to_string()))?;
    let mut meta_json = Vec::new();
    let mut payload = Vec::new();
    for i in 0..archive.len() {
        let mut file = archive.by_index(i).map_err(|e| AppError::Archive(e.to_string()))?;
        match file.name() {
            METADATA_ENTRY => {
                file.read_to_end(&mut meta_json)
                    .map_err(|e| AppError::Archive(e.to_string()))?;
            }
            PAYLOAD_ENTRY => {
                file.read_to_end(&mut payload)
                    .map_err(|e| AppError::Archive(e.to_string()))?;
            }
            _ => {}
        }
    }
    let meta: ArchiveMetadata = serde_json::from_slice(&meta_json)?;
    Ok((meta, payload))
}

async fn unseal(
    meta: &ArchiveMetadata,
    payload: &[u8],
    password: &str,
    cipher: CipherType,
    report: &mut Reporter,
) -> Result<Vec<u8>, AppError> {
    let parts = StreamParts::recover(password, cipher, &meta.salt, &meta.nonce)?;
    let mut inner = Vec::with_capacity(payload.len());
    let mut offset = 0usize;
    let mut position = 0u32;
    while offset < payload.len() {
        let end = (offset + STREAM_CHUNK + STREAM_TAG).min(payload.len());
        let last = end == payload.len();
        inner.extend(parts.decrypt_chunk(position, last, &payload[offset..end])?);
        offset = end;
        position += 1;
        report(Job {
            stage: Stage::Decrypt,
            done: offset as u64,
            total: payload.len() as u64,
            name: None,
        });
        yield_to_paint().await;
    }
    Ok(inner)
}

async fn unzip_inner(inner: Vec<u8>, report: &mut Reporter) -> Result<(String, Vec<Attachment>), AppError> {
    let mut archive =
        zip::ZipArchive::new(std::io::Cursor::new(inner)).map_err(|e| AppError::Archive(e.to_string()))?;
    let total = (0..archive.len())
        .map(|i| archive.by_index(i).map(|f| f.size()).unwrap_or(0))
        .sum::<u64>();
    let mut note = String::new();
    let mut files = Vec::new();
    let mut done = 0u64;
    for i in 0..archive.len() {
        let (name, size, data) = {
            let mut file = archive.by_index(i).map_err(|e| AppError::Archive(e.to_string()))?;
            let name = file.name().to_string();
            let size = file.size();
            let mut data = Vec::new();
            file.read_to_end(&mut data)
                .map_err(|e| AppError::Archive(e.to_string()))?;
            (name, size, data)
        };
        done += size;
        if name == "note.txt" {
            note = String::from_utf8(data).map_err(AppError::Utf8)?;
        } else {
            files.push(Attachment {
                name: name.strip_prefix("attachments/").unwrap_or(&name).to_string(),
                data,
            });
        }
        report(Job {
            stage: Stage::Unzip,
            done,
            total,
            name: Some(name.strip_prefix("attachments/").unwrap_or(&name).to_string()),
        });
        yield_to_paint().await;
    }
    Ok((note, files))
}
