use crate::crypto::*;
use crate::error::*;
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

fn opts(method: CompressionMethod) -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default().compression_method(method)
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
    let mut files = vec![Attachment {
        name: "note.txt".into(),
        data: note.as_bytes().to_vec(),
    }];
    for att in attachments {
        files.push(Attachment {
            name: format!("attachments/{}", att.name),
            data: att.data.clone(),
        });
    }
    create_zip(&files)
}

pub fn create_archive_package(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
) -> Result<Vec<u8>, AppError> {
    let inner = create_inner_zip(note, attachments)?;
    let (payload, meta) = match cipher {
        Some(cipher) => {
            let encrypted = encrypt_symmetric(&inner, password, cipher)?;
            (
                encrypted.ciphertext,
                ArchiveMetadata {
                    cipher: Some(encrypted.cipher),
                    kdf: Kdf::Argon2id,
                    nonce: encrypted.nonce,
                    salt: encrypted.salt,
                },
            )
        }
        None => (
            inner,
            ArchiveMetadata {
                cipher: None,
                kdf: Kdf::Argon2id,
                nonce: Vec::new(),
                salt: Vec::new(),
            },
        ),
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
        zip.write_all(&payload).map_err(|e| AppError::Archive(e.to_string()))?;
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

pub fn extract_archive_package(package: &[u8], password: &str) -> Result<(String, Vec<Attachment>), AppError> {
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
    let inner = match meta.cipher {
        Some(cipher) => decrypt_symmetric(
            &EncryptedNote {
                cipher,
                nonce: meta.nonce,
                ciphertext: payload,
                salt: meta.salt,
                kdf: meta.kdf,
            },
            password,
        )?,
        None => payload,
    };
    let mut inner = zip::ZipArchive::new(std::io::Cursor::new(inner)).map_err(|e| AppError::Archive(e.to_string()))?;
    let mut note = String::new();
    let mut files = Vec::new();
    for i in 0..inner.len() {
        let mut file = inner.by_index(i).map_err(|e| AppError::Archive(e.to_string()))?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)
            .map_err(|e| AppError::Archive(e.to_string()))?;
        let name = file.name().to_string();
        if name == "note.txt" {
            note = String::from_utf8(data).map_err(AppError::Utf8)?;
        } else {
            let clean = name.strip_prefix("attachments/").unwrap_or(&name).to_string();
            files.push(Attachment { name: clean, data });
        }
    }
    Ok((note, files))
}
