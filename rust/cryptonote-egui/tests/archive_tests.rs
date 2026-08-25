#![cfg(not(target_arch = "wasm32"))]
#![allow(clippy::unwrap_used, clippy::expect_used)]
mod common;
use cryptonote_egui::AppError;
use cryptonote_egui::archive::{
    ArchiveMetadata, ArchiveSource, create_archive_package, extract_archive_package,
    read_archive_metadata,
};
use cryptonote_egui::crypto::CipherType;
use cryptonote_egui::task::Reporter;
use functora_core::files::Attachment;
use std::sync::Arc;

fn progress() -> Reporter {
    Box::new(|_| {})
}

fn roundtrip(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
) -> Result<(String, Vec<Attachment>), AppError> {
    let mut report = progress();
    let pkg = futures_executor::block_on(create_archive_package(
        note,
        attachments,
        password,
        cipher,
        &mut report,
    ))?;
    let mut extract_report = progress();
    futures_executor::block_on(extract_archive_package(
        ArchiveSource::Bytes(pkg),
        password,
        &mut extract_report,
    ))
}

#[test]
fn test_archive_roundtrip_chacha20() {
    common::fast_kdf();
    let note = "This is a chacha20 encrypted note with attachments";
    let attachments = vec![
        Attachment {
            name: "hello.txt".into(),
            data: Arc::from(b"Hello, World!".as_slice()),
        },
        Attachment {
            name: "data.bin".into(),
            data: Arc::from(vec![1u8, 2, 3, 4, 5]),
        },
    ];
    let result = roundtrip(
        note,
        &attachments,
        "password",
        Some(CipherType::ChaCha20Poly1305),
    )
    .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, note);
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].name, "hello.txt");
    assert_eq!(files[0].data.as_ref(), b"Hello, World!");
    assert_eq!(files[1].name, "data.bin");
    assert_eq!(files[1].data.as_ref(), vec![1, 2, 3, 4, 5]);
}

#[test]
fn test_archive_roundtrip_aes() {
    common::fast_kdf();
    let attachments = vec![Attachment {
        name: "file.txt".into(),
        data: Arc::from(b"content".as_slice()),
    }];
    let result = roundtrip(
        "AES note",
        &attachments,
        "password",
        Some(CipherType::Aes256Gcm),
    )
    .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, "AES note");
    assert_eq!(files[0].name, "file.txt");
    assert_eq!(files[0].data.as_ref(), b"content");
}

#[test]
fn test_archive_no_attachments() {
    common::fast_kdf();
    let result = roundtrip("Just a note", &[], "password", Some(CipherType::Aes256Gcm))
        .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, "Just a note");
    assert!(files.is_empty());
}

#[test]
fn test_archive_wrong_password() {
    common::fast_kdf();
    let result = roundtrip("note", &[], "password", Some(CipherType::ChaCha20Poly1305))
        .expect("Archive roundtrip failed");
    let (text, _files) = result;
    assert_eq!(text, "note");
    assert!(
        futures_executor::block_on(extract_archive_package(
            ArchiveSource::Bytes(vec![1, 2, 3]),
            "wrong",
            &mut progress(),
        ))
        .is_err()
    );
}

#[test]
fn test_archive_plaintext_roundtrip() {
    common::fast_kdf();
    let attachments = vec![
        Attachment {
            name: "a.bin".into(),
            data: Arc::from(vec![9u8, 9, 9]),
        },
        Attachment {
            name: "b.txt".into(),
            data: Arc::from(b"plain".as_slice()),
        },
    ];
    let result = roundtrip(
        "Plain note with attachments",
        &attachments,
        "password",
        None,
    )
    .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, "Plain note with attachments");
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].data.as_ref(), vec![9, 9, 9]);
    assert_eq!(files[1].data.as_ref(), b"plain");
}

#[test]
fn test_archive_empty_note() {
    common::fast_kdf();
    let result = roundtrip("", &[], "password", Some(CipherType::Aes256Gcm))
        .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, "");
    assert!(files.is_empty());
}

#[test]
fn test_archive_many_attachments() {
    common::fast_kdf();
    let note = "note with many attachments";
    let attachments: Vec<Attachment> = (0..100)
        .map(|i| Attachment {
            name: format!("file_{i}.bin"),
            data: Arc::from(vec![u8::try_from(i).unwrap_or(u8::MAX); 100]),
        })
        .collect();
    let result = roundtrip(note, &attachments, "password", Some(CipherType::Aes256Gcm))
        .expect("Archive roundtrip failed");
    let (text, files) = result;
    assert_eq!(text, note);
    assert_eq!(files.len(), 100);
    for (i, f) in files.iter().enumerate() {
        assert_eq!(f.name, format!("file_{i}.bin"));
        assert_eq!(f.data.len(), 100);
    }
}

#[test]
fn test_archive_metadata_serde_roundtrip() {
    let meta = ArchiveMetadata {
        cipher: Some(CipherType::ChaCha20Poly1305),
        kdf: functora_core::crypto::Kdf::Argon2id,
        nonce: vec![10; 12],
        salt: vec![20; 32],
    };
    let json = serde_json::to_string(&meta).unwrap();
    assert!(!json.contains("version"));
    let back: ArchiveMetadata = serde_json::from_str(&json).unwrap();
    assert_eq!(meta, back);
}

#[test]
fn test_plaintext_archive_metadata_is_none() {
    common::fast_kdf();
    let mut report = progress();
    let pkg = futures_executor::block_on(create_archive_package(
        "note",
        &[],
        "password",
        None,
        &mut report,
    ))
    .expect("Package creation failed");
    let meta = read_archive_metadata(&ArchiveSource::Bytes(pkg)).expect("Metadata read failed");
    assert!(meta.cipher.is_none());
}

#[test]
fn test_encrypted_archive_metadata_cipher() {
    common::fast_kdf();
    let mut report = progress();
    let pkg = futures_executor::block_on(create_archive_package(
        "note",
        &[],
        "password",
        Some(CipherType::Aes256Gcm),
        &mut report,
    ))
    .expect("Package creation failed");
    let meta = read_archive_metadata(&ArchiveSource::Bytes(pkg)).expect("Metadata read failed");
    assert_eq!(meta.cipher, Some(CipherType::Aes256Gcm));
}
