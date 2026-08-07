use crate::Error;
use crate::crypto::{CipherType, Kdf, STREAM_CHUNK, STREAM_TAG, StreamParts};
use crate::progress::{Job, yield_to_paint};
use crate::worker::{Reporter, run};
use crate::zip::zip_entries;
use dioxus::prelude::Writable;
use serde::{Deserialize, Serialize};
use std::io::{Read, Seek, Write};
use std::path::PathBuf;
use tap::prelude::*;
use zip::CompressionMethod;

pub const METADATA_ENTRY: &str = "metadata.json";
pub const PAYLOAD_ENTRY: &str = "payload.cpt";

pub fn aad(prefix: &[u8], cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    let mut aad = prefix.to_vec();
    aad.push(cipher as u8);
    aad.push(kdf as u8);
    aad
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

trait SeekableRead: Read + Seek {}

impl<T: Read + Seek> SeekableRead for T {}

impl ArchiveSource {
    fn open(&self) -> Result<Box<dyn SeekableRead + '_>, Error> {
        match self {
            Self::Bytes(bytes) => Ok(Box::new(std::io::Cursor::new(bytes.as_slice()))),
            Self::Path(path) => std::fs::File::open(path)
                .map(|file| Box::new(file) as Box<dyn SeekableRead>)
                .map_err(|e| Error::Archive(e.to_string())),
        }
    }

    pub fn into_bytes(self) -> Result<Vec<u8>, Error> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            Self::Path(path) => std::fs::read(path).map_err(|e| Error::Archive(e.to_string())),
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
        Self { zip, encrypt, decrypt }
    }
}

fn stored_opts() -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default().compression_method(CompressionMethod::Stored)
}

fn write_package_entries(
    zip: &mut zip::ZipWriter<std::io::Cursor<&mut Vec<u8>>>,
    meta: &ArchiveMetadata,
) -> Result<(), Error> {
    zip.start_file(METADATA_ENTRY, stored_opts())
        .map_err(|e| Error::Archive(e.to_string()))?;
    zip.write_all(&serde_json::to_vec(meta)?)
        .map_err(|e| Error::Archive(e.to_string()))?;
    zip.start_file(PAYLOAD_ENTRY, stored_opts())
        .map_err(|e| Error::Archive(e.to_string()))?;
    Ok(())
}

pub async fn package_report<S>(
    entries: Vec<(String, Vec<u8>)>,
    password: &str,
    cipher: Option<CipherType>,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let total = entries.iter().map(|(_, d)| d.len() as u64).sum::<u64>();
    let mut inner = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut inner));
        let mut done = 0u64;
        zip_entries(&mut zip, &entries, stages.zip, report, &mut done, total).await?;
        _ = zip.finish().map_err(|e| Error::Archive(e.to_string()))?;
    }
    match cipher {
        Some(cipher_sel) => package_with_encryption(&inner, password, cipher_sel, prefix, stages, report).await,
        None => package_plain(&inner, stages, report).await,
    }
}

async fn package_with_encryption<S>(
    inner: &[u8],
    password: &str,
    cipher: CipherType,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
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
        let mut offset = 0usize;
        let mut position = 0u32;
        while offset < inner.len() {
            let end = (offset + STREAM_CHUNK).min(inner.len());
            let last = end == inner.len();
            let ct = parts.encrypt_chunk(position, last, &inner[offset..end])?;
            zip.write_all(&ct).map_err(|e| Error::Archive(e.to_string()))?;
            offset = end;
            position += 1;
            report(Job {
                stage: stages.encrypt,
                done: offset as u64,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        _ = zip.finish().map_err(|e| Error::Archive(e.to_string()))?;
    }
    Ok(pkg)
}

async fn package_plain<S>(inner: &[u8], stages: PackageStages<S>, report: &mut Reporter<S>) -> Result<Vec<u8>, Error>
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
        for chunk in inner.chunks(2 * 1024 * 1024) {
            zip.write_all(chunk).map_err(|e| Error::Archive(e.to_string()))?;
            done += chunk.len() as u64;
            report(Job {
                stage: stages.zip,
                done,
                total: inner.len() as u64,
                name: None,
            });
            yield_to_paint().await;
        }
        _ = zip.finish().map_err(|e| Error::Archive(e.to_string()))?;
    }
    Ok(pkg)
}

pub async fn package_async<P, S>(
    entries: Vec<(String, Vec<u8>)>,
    password: &str,
    cipher: Option<CipherType>,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    progress: P,
) -> Result<Vec<u8>, Error>
where
    P: Writable<Target = Option<Job<S>>> + 'static,
    S: Copy + Send + Sync + 'static,
{
    let password_owned = password.to_string();
    run(
        (entries, password_owned, cipher),
        progress,
        move |(entries_owned, password_clone, cipher_sel), mut report| async move {
            package_report(entries_owned, &password_clone, cipher_sel, prefix, stages, &mut report).await
        },
    )
    .await
}

pub fn read_metadata(source: &ArchiveSource) -> Result<ArchiveMetadata, Error> {
    let mut archive = zip::ZipArchive::new(source.open()?).map_err(|e| Error::Archive(e.to_string()))?;
    let mut meta_file = archive
        .by_name(METADATA_ENTRY)
        .map_err(|e| Error::Archive(e.to_string()))?;
    let mut meta_json = Vec::new();
    _ = meta_file
        .read_to_end(&mut meta_json)
        .map_err(|e| Error::Archive(e.to_string()))?;
    serde_json::from_slice::<ArchiveMetadata>(&meta_json)?.pipe(Ok)
}

fn read_package(source: &ArchiveSource) -> Result<(ArchiveMetadata, Vec<u8>), Error> {
    let mut archive = zip::ZipArchive::new(source.open()?).map_err(|e| Error::Archive(e.to_string()))?;
    let mut meta_json = Vec::new();
    let mut payload = Vec::new();
    for i in 0..archive.len() {
        let mut file = archive.by_index(i).map_err(|e| Error::Archive(e.to_string()))?;
        match file.name() {
            METADATA_ENTRY => {
                _ = file
                    .read_to_end(&mut meta_json)
                    .map_err(|e| Error::Archive(e.to_string()))?;
            }
            PAYLOAD_ENTRY => {
                payload = Vec::with_capacity(usize::try_from(file.size()).unwrap_or_default());
                _ = file
                    .read_to_end(&mut payload)
                    .map_err(|e| Error::Archive(e.to_string()))?;
            }
            _ => {}
        }
    }
    let meta: ArchiveMetadata = serde_json::from_slice(&meta_json)?;
    Ok((meta, payload))
}

async fn unseal_report<S>(
    meta: &ArchiveMetadata,
    payload: Vec<u8>,
    password: &str,
    cipher: CipherType,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    report: &mut Reporter<S>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let parts = StreamParts::recover(
        password,
        cipher,
        &meta.salt,
        &meta.nonce,
        &aad(prefix, cipher, meta.kdf),
    )?;
    let mut inner = payload;
    let mut offset = 0usize;
    let mut write = 0usize;
    let mut position = 0u32;
    while offset < inner.len() {
        let end = (offset + STREAM_CHUNK + STREAM_TAG).min(inner.len());
        let last = end == inner.len();
        let plain = parts.decrypt_chunk(position, last, &inner[offset..end])?;
        inner[write..write + plain.len()].copy_from_slice(&plain);
        offset = end;
        write += plain.len();
        position += 1;
        report(Job {
            stage: stages.decrypt,
            done: offset as u64,
            total: inner.len() as u64,
            name: None,
        });
        yield_to_paint().await;
    }
    inner.truncate(write);
    Ok(inner)
}

pub async fn extract_package_async<P, S>(
    source: ArchiveSource,
    password: &str,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    progress: P,
) -> Result<Vec<u8>, Error>
where
    P: Writable<Target = Option<Job<S>>> + 'static,
    S: Copy + Send + Sync + 'static,
{
    let password_owned = password.to_string();
    run(source, progress, move |source_owned, mut report| async move {
        let (meta, payload) = read_package(&source_owned)?;
        match meta.cipher {
            Some(cipher) => unseal_report(&meta, payload, &password_owned, cipher, prefix, stages, &mut report).await,
            None => Ok(payload),
        }
    })
    .await
}
