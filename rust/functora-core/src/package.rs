use crate::Error;
use crate::crypto::{CipherType, Kdf, STREAM_CHUNK, STREAM_TAG, StreamParts, stream_spans};
use crate::progress::{Job, yield_to_paint};
use crate::worker::Reporter;
use crate::zip::{CancelToken, ZIP_CHUNK, check_cancel, zip_entries};
use serde::{Deserialize, Serialize};
use std::io::{Read, Seek, Write};
use std::path::PathBuf;
use std::sync::Arc;
use tap::prelude::*;
use zip::CompressionMethod;

pub const METADATA_ENTRY: &str = "metadata.json";
pub const PAYLOAD_ENTRY: &str = "payload.cpt";

#[must_use]
pub fn aad(prefix: &[u8], cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    prefix
        .iter()
        .chain(&[cipher as u8, kdf as u8])
        .copied()
        .collect()
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArchiveMetadata {
    pub cipher: Option<CipherType>,
    pub kdf: Kdf,
    pub nonce: Vec<u8>,
    pub salt: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArchiveSource {
    Bytes(Vec<u8>),
    Path(PathBuf),
}

impl ArchiveSource {
    pub fn into_bytes(self) -> Result<Vec<u8>, Error> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Path(path) => Ok(std::fs::read(path)?),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PackageStages<S> {
    pub zip: S,
    pub encrypt: S,
    pub decrypt: S,
}

impl<S> PackageStages<S> {
    pub const fn new(zip: S, encrypt: S, decrypt: S) -> Self {
        Self {
            zip,
            encrypt,
            decrypt,
        }
    }
}

fn stored_opts() -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default().compression_method(CompressionMethod::Stored)
}

fn write_package_entries(
    zip: &mut zip::ZipWriter<std::io::Cursor<&mut Vec<u8>>>,
    meta: &ArchiveMetadata,
) -> Result<(), Error> {
    zip.start_file(METADATA_ENTRY, stored_opts())?;
    zip.write_all(&serde_json::to_vec(meta)?)?;
    zip.start_file(PAYLOAD_ENTRY, stored_opts())?;
    Ok(())
}

pub async fn package_report<S>(
    entries: Vec<(String, Arc<[u8]>)>,
    password: &str,
    cipher: Option<CipherType>,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let total = entries.iter().map(|(_, d)| d.len() as u64).sum::<u64>();
    let mut inner = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut inner));
        let mut done = 0u64;
        zip_entries(
            &mut zip, &entries, stages.zip, report, &mut done, total, cancel,
        )
        .await?;
        let _ = zip.finish()?;
    }
    match cipher {
        Some(cipher_sel) => {
            package_with_encryption(&inner, password, cipher_sel, prefix, stages, report, cancel)
                .await
        }
        None => package_plain(&inner, stages, report, cancel).await,
    }
}

async fn package_with_encryption<S>(
    inner: &[u8],
    password: &str,
    cipher: CipherType,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let parts = StreamParts::derive(password, cipher, &aad(prefix, cipher, Kdf::Argon2id))?;
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
        for span in stream_spans(inner.len(), STREAM_CHUNK) {
            check_cancel(cancel)?;
            let (position, kind, offset, end) = span?;
            zip.write_all(&parts.encrypt_chunk(position, kind, &inner[offset..end])?)?;
            report(Job {
                stage: stages.encrypt,
                done: end as u64,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        let _ = zip.finish()?;
    }
    Ok(pkg)
}

async fn package_plain<S>(
    inner: &[u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
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
            check_cancel(cancel)?;
            zip.write_all(chunk)?;
            done += chunk.len() as u64;
            report(Job {
                stage: stages.zip,
                done,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        let _ = zip.finish()?;
    }
    Ok(pkg)
}

fn read_entry<R: Read + Seek>(
    archive: &mut zip::ZipArchive<R>,
    name: &str,
) -> Result<Vec<u8>, Error> {
    let mut file = archive.by_name(name)?;
    let capacity = usize::try_from(file.size()).map_err(|e| Error::Convert {
        context: "entry size too large",
        source: e,
    })?;
    let mut data = Vec::with_capacity(capacity);
    let _ = file.read_to_end(&mut data)?;
    Ok(data)
}

fn metadata_from_reader<R: Read + Seek>(reader: R) -> Result<ArchiveMetadata, Error> {
    let mut archive = zip::ZipArchive::new(reader)?;
    let json = read_entry(&mut archive, METADATA_ENTRY)?;
    serde_json::from_slice::<ArchiveMetadata>(&json)?.pipe(Ok)
}

pub fn read_metadata(source: &ArchiveSource) -> Result<ArchiveMetadata, Error> {
    match source {
        ArchiveSource::Bytes(bytes) => metadata_from_reader(std::io::Cursor::new(bytes.as_slice())),
        ArchiveSource::Path(path) => metadata_from_reader(std::fs::File::open(path)?),
    }
}

fn package_from_reader<R: Read + Seek>(reader: R) -> Result<(ArchiveMetadata, Vec<u8>), Error> {
    let mut archive = zip::ZipArchive::new(reader)?;
    let meta: ArchiveMetadata = serde_json::from_slice(&read_entry(&mut archive, METADATA_ENTRY)?)?;
    let payload = read_entry(&mut archive, PAYLOAD_ENTRY)?;
    (meta, payload).pipe(Ok)
}

fn read_package(source: &ArchiveSource) -> Result<(ArchiveMetadata, Vec<u8>), Error> {
    match source {
        ArchiveSource::Bytes(bytes) => package_from_reader(std::io::Cursor::new(bytes.as_slice())),
        ArchiveSource::Path(path) => package_from_reader(std::fs::File::open(path)?),
    }
}

struct UnsealParams<'a> {
    meta: &'a ArchiveMetadata,
    password: &'a str,
    cipher: CipherType,
    prefix: &'static [u8],
}

async fn unseal_report<S>(
    params: UnsealParams<'_>,
    payload: Vec<u8>,
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let parts = StreamParts::recover(
        params.password,
        params.cipher,
        &params.meta.salt,
        &params.meta.nonce,
        &aad(params.prefix, params.cipher, params.meta.kdf),
    )?;
    let total = payload.len();
    let mut out = Vec::with_capacity(payload.len());
    for span in stream_spans(total, STREAM_CHUNK + STREAM_TAG) {
        check_cancel(cancel)?;
        let (position, kind, offset, end) = span?;
        out.extend_from_slice(&parts.decrypt_chunk(position, kind, &payload[offset..end])?);
        report(Job {
            stage: stages.decrypt,
            done: end as u64,
            total: total as u64,
            name: None,
        });
        yield_to_paint().await;
    }
    Ok(out)
}

pub async fn extract_package_report<S>(
    source: ArchiveSource,
    password: &str,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let (meta, payload) = read_package(&source)?;
    match meta.cipher {
        Some(cipher) => {
            unseal_report(
                UnsealParams {
                    meta: &meta,
                    password,
                    cipher,
                    prefix,
                },
                payload,
                stages,
                report,
                cancel,
            )
            .await
        }
        None => Ok(payload),
    }
}
