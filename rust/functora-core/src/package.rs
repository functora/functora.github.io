use crate::Error;
use crate::crypto::{CipherType, Kdf, STREAM_CHUNK, STREAM_TAG, StreamParts, stream_spans};
use crate::progress::{Job, yield_to_paint};
use crate::worker::Reporter;
use crate::zip::{
    CancelToken, MAX_ARCHIVE_ENTRY_SIZE, ZIP_CHUNK, check_cancel, check_entry_size, zip_entries,
};
use serde::{Deserialize, Serialize};
use std::io::{Read, Seek, Write};
use std::path::PathBuf;
use std::sync::Arc;
use tap::prelude::*;
use zip::CompressionMethod;

pub const METADATA_ENTRY: &str = "metadata.json";
pub const PAYLOAD_ENTRY: &str = "payload.cpt";

/// The frame of a payload that was written without encryption.
pub const PLAIN_PAYLOAD_FRAME: &[u8] = b"cryptonote-plain";
/// The frame of a payload that was written encrypted. The frame is also
/// bound as AEAD associated data, so the ciphertext is authenticated
/// together with the branch it belongs to.
pub const ENCRYPTED_PAYLOAD_FRAME: &[u8] = b"cryptonote-encry";
/// The length of the frame that opens every payload.
pub const PAYLOAD_FRAME_LEN: usize = PLAIN_PAYLOAD_FRAME.len();

const _: () = assert!(ENCRYPTED_PAYLOAD_FRAME.len() == PAYLOAD_FRAME_LEN);

/// The branch an archive payload was written in. The frame in the payload
/// names it, so an edited metadata file cannot move an archive into the
/// other branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PayloadBranch {
    Plain,
    Encrypted,
}

impl PayloadBranch {
    #[must_use]
    fn frame(self) -> &'static [u8] {
        match self {
            Self::Plain => PLAIN_PAYLOAD_FRAME,
            Self::Encrypted => ENCRYPTED_PAYLOAD_FRAME,
        }
    }
}

#[must_use]
pub fn aad(prefix: &[u8], cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    prefix
        .iter()
        .chain(&[cipher as u8, kdf as u8])
        .copied()
        .collect()
}

/// The associated data of an encrypted payload. It binds the application
/// prefix, the cipher, the KDF and the branch frame, so a payload cannot be
/// replayed into another branch or another application.
fn payload_aad(prefix: &[u8], cipher: CipherType, kdf: Kdf, frame: &[u8]) -> Vec<u8> {
    aad(prefix, cipher, kdf)
        .into_iter()
        .chain(frame.iter().copied())
        .collect()
}

fn frame_branch(frame: &[u8]) -> Result<PayloadBranch, Error> {
    [PayloadBranch::Plain, PayloadBranch::Encrypted]
        .into_iter()
        .find(|branch| frame == branch.frame())
        .ok_or_else(|| {
            Error::InvalidFormat("archive payload does not carry a known branch frame".into())
        })
}

/// Check that the metadata agrees with the branch the payload was written
/// in. The branch comes from the payload frame, so an edited metadata file
/// cannot move an archive into the other branch.
fn check_branch(branch: PayloadBranch, meta: &ArchiveMetadata) -> Result<(), Error> {
    match (branch, meta.cipher) {
        (PayloadBranch::Plain, None)
        | (PayloadBranch::Encrypted, Some(CipherType::Aes256Gcm | CipherType::ChaCha20Poly1305)) => {
            Ok(())
        }
        (PayloadBranch::Plain, Some(CipherType::Aes256Gcm | CipherType::ChaCha20Poly1305))
        | (PayloadBranch::Encrypted, None) => Err(Error::InvalidFormat(
            "archive metadata does not match the payload branch".into(),
        )),
    }
}

/// A validated view of an archive before it is opened. The branch comes
/// from the payload frame, and the metadata has to agree with it.
#[derive(Debug, Clone, PartialEq)]
pub struct ArchiveHeader {
    pub metadata: ArchiveMetadata,
    pub branch: PayloadBranch,
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

fn stored_opts() -> zip::write::FileOptions<'static, 'static, ()> {
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
    let parts = StreamParts::derive(
        password,
        cipher,
        &payload_aad(prefix, cipher, Kdf::Argon2id, ENCRYPTED_PAYLOAD_FRAME),
    )?;
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
        zip.write_all(ENCRYPTED_PAYLOAD_FRAME)?;
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
        zip.write_all(PLAIN_PAYLOAD_FRAME)?;
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

fn read_bounded<R: Read>(reader: R, name: &str, declared: u64) -> Result<Vec<u8>, Error> {
    let capacity = usize::try_from(declared.min(ZIP_CHUNK as u64)).map_err(|e| Error::Convert {
        context: "zip entry capacity exceeds usize range",
        source: e,
    })?;
    let mut data = Vec::with_capacity(capacity);
    let read = reader
        .take(MAX_ARCHIVE_ENTRY_SIZE + 1)
        .read_to_end(&mut data)?;
    check_entry_size(name, read as u64)?;
    Ok(data)
}

fn read_entry<R: Read + Seek>(
    archive: &mut zip::ZipArchive<R>,
    name: &str,
) -> Result<Vec<u8>, Error> {
    let file = archive.by_name(name)?;
    let size = file.size();
    check_entry_size(name, size)?;
    read_bounded(file, name, size)
}

/// Read the branch frame that opens every payload.
fn read_frame_bytes<R: Read>(reader: &mut R) -> Result<PayloadBranch, Error> {
    let mut frame = [0u8; PAYLOAD_FRAME_LEN];
    reader.read_exact(&mut frame)?;
    frame_branch(&frame)
}

/// Read the payload entry and split it into the branch frame and the body
/// the branch carries.
fn read_payload<R: Read + Seek>(
    archive: &mut zip::ZipArchive<R>,
) -> Result<(PayloadBranch, Vec<u8>), Error> {
    let mut file = archive.by_name(PAYLOAD_ENTRY)?;
    let size = file.size();
    check_entry_size(PAYLOAD_ENTRY, size)?;
    let branch = read_frame_bytes(&mut file)?;
    read_bounded(
        file,
        PAYLOAD_ENTRY,
        size.saturating_sub(PAYLOAD_FRAME_LEN as u64),
    )
    .map(|body| (branch, body))
}

/// Read the branch frame of the payload entry.
fn read_frame<R: Read + Seek>(archive: &mut zip::ZipArchive<R>) -> Result<PayloadBranch, Error> {
    let mut file = archive.by_name(PAYLOAD_ENTRY)?;
    read_frame_bytes(&mut file)
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

/// Read the metadata of an archive together with the branch its payload was
/// written in. The branch comes from the payload frame, and the metadata has
/// to agree with it, so an edited metadata file cannot move an archive into
/// the other branch.
pub fn read_header(source: &ArchiveSource) -> Result<ArchiveHeader, Error> {
    match source {
        ArchiveSource::Bytes(bytes) => header_from_reader(std::io::Cursor::new(bytes.as_slice())),
        ArchiveSource::Path(path) => header_from_reader(std::fs::File::open(path)?),
    }
}

fn header_from_reader<R: Read + Seek>(reader: R) -> Result<ArchiveHeader, Error> {
    let mut archive = zip::ZipArchive::new(reader)?;
    let metadata: ArchiveMetadata =
        serde_json::from_slice(&read_entry(&mut archive, METADATA_ENTRY)?)?;
    let branch = read_frame(&mut archive)?;
    check_branch(branch, &metadata)?;
    Ok(ArchiveHeader { metadata, branch })
}

fn package_from_reader<R: Read + Seek>(
    reader: R,
) -> Result<(ArchiveMetadata, PayloadBranch, Vec<u8>), Error> {
    let mut archive = zip::ZipArchive::new(reader)?;
    let meta: ArchiveMetadata = serde_json::from_slice(&read_entry(&mut archive, METADATA_ENTRY)?)?;
    let (branch, payload) = read_payload(&mut archive)?;
    (meta, branch, payload).pipe(Ok)
}

fn read_package(
    source: &ArchiveSource,
) -> Result<(ArchiveMetadata, PayloadBranch, Vec<u8>), Error> {
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
        params.meta.kdf,
        &params.meta.salt,
        &params.meta.nonce,
        &payload_aad(
            params.prefix,
            params.cipher,
            params.meta.kdf,
            ENCRYPTED_PAYLOAD_FRAME,
        ),
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
    let (meta, branch, payload) = read_package(&source)?;
    check_branch(branch, &meta)?;
    match branch {
        PayloadBranch::Plain => Ok(payload),
        PayloadBranch::Encrypted => {
            let Some(cipher) = meta.cipher else {
                return Err(Error::InvalidFormat(
                    "archive metadata does not record a cipher".into(),
                ));
            };
            if password.is_empty() {
                return Err(Error::PasswordRequired);
            }
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
    }
}
