use crate::crypto::*;
use crate::error::*;
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use tap::prelude::*;
use zip::CompressionMethod;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FileEntry {
    pub name: String,
    pub size: u64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArchiveMetadata {
    pub cipher: CipherType,
    pub nonce: Vec<u8>,
    pub salt: Vec<u8>,
    pub files: Vec<FileEntry>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attachment {
    pub name: String,
    pub data: Vec<u8>,
}

const METADATA_ENTRY: &str = "_metadata.json";
const PAYLOAD_ENTRY: &str = "_payload";

fn opts(method: CompressionMethod) -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default().compression_method(method)
}

fn collect_files(note: &str, attachments: &[Attachment]) -> Vec<FileEntry> {
    std::iter::once(FileEntry {
        name: "_note.txt".into(),
        size: note.len() as u64,
    })
    .chain(attachments.iter().map(|a| FileEntry {
        name: a.name.clone(),
        size: a.data.len() as u64,
    }))
    .collect()
}

pub fn create_zip(files: &[Attachment]) -> Result<Vec<u8>, AppError> {
    let mut buf = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut buf));
        let deflated = opts(CompressionMethod::Deflated);
        for f in files {
            zip.start_file(&f.name, deflated)
                .map_err(|e| AppError::Archive(e.to_string()))?;
            zip.write_all(&f.data).map_err(|e| AppError::Archive(e.to_string()))?;
        }
        zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
    }
    Ok(buf)
}

fn create_inner_zip(note: &str, attachments: &[Attachment]) -> Result<Vec<u8>, AppError> {
    create_zip(
        &[Attachment {
            name: "_note.txt".into(),
            data: note.as_bytes().to_vec(),
        }]
        .into_iter()
        .chain(attachments.iter().cloned())
        .collect::<Vec<_>>(),
    )
}

pub fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: CipherType,
) -> Result<Vec<u8>, AppError> {
    let inner = create_inner_zip(note, attachments)?;
    let encrypted = encrypt_symmetric(&inner, password, cipher)?;
    let files = collect_files(note, attachments);
    let meta = ArchiveMetadata {
        cipher: encrypted.cipher,
        nonce: encrypted.nonce,
        salt: encrypted.salt,
        files,
    };
    let meta_json = serde_json::to_vec(&meta)?;
    let mut pkg = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut pkg));
        let stored = opts(CompressionMethod::Stored);
        zip.start_file(METADATA_ENTRY, stored)
            .map_err(|e| AppError::Archive(e.to_string()))?;
        zip.write_all(&meta_json)
            .map_err(|e| AppError::Archive(e.to_string()))?;
        zip.start_file(PAYLOAD_ENTRY, stored)
            .map_err(|e| AppError::Archive(e.to_string()))?;
        zip.write_all(&encrypted.ciphertext)
            .map_err(|e| AppError::Archive(e.to_string()))?;
        zip.finish().map_err(|e| AppError::Archive(e.to_string()))?;
    }
    Ok(pkg)
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

pub fn extract_archive_package(package: &[u8], password: &str) -> Result<Vec<Attachment>, AppError> {
    let mut archive =
        zip::ZipArchive::new(std::io::Cursor::new(package)).map_err(|e| AppError::Archive(e.to_string()))?;
    let mut meta_json = Vec::new();
    let mut ciphertext = Vec::new();
    for i in 0..archive.len() {
        let mut file = archive.by_index(i).map_err(|e| AppError::Archive(e.to_string()))?;
        match file.name() {
            METADATA_ENTRY => {
                file.read_to_end(&mut meta_json)
                    .map_err(|e| AppError::Archive(e.to_string()))?;
            }
            PAYLOAD_ENTRY => {
                file.read_to_end(&mut ciphertext)
                    .map_err(|e| AppError::Archive(e.to_string()))?;
            }
            _ => {}
        }
    }
    let meta: ArchiveMetadata = serde_json::from_slice(&meta_json)?;
    let decrypted = decrypt_symmetric(
        &EncryptedData {
            cipher: meta.cipher,
            nonce: meta.nonce,
            ciphertext,
            salt: meta.salt,
        },
        password,
    )?;
    let mut inner =
        zip::ZipArchive::new(std::io::Cursor::new(decrypted)).map_err(|e| AppError::Archive(e.to_string()))?;
    (0..inner.len())
        .map(|i| {
            let mut file = inner.by_index(i).map_err(|e| AppError::Archive(e.to_string()))?;
            let name = file.name().to_string();
            let mut data = Vec::new();
            file.read_to_end(&mut data)
                .map_err(|e| AppError::Archive(e.to_string()))?;
            Ok(Attachment { name, data })
        })
        .collect()
}
