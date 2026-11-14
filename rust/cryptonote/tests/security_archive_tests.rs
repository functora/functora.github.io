//! The `.cryptonote` archive format must not let an untrusted archive steer
//! the app into an unauthenticated branch, and it must not carry path
//! traversal in the attachment names it hands back to the file system.

#![cfg(not(target_arch = "wasm32"))]

pub mod common;
#[cfg(not(target_arch = "wasm32"))]
use common::run_async;
#[cfg(not(target_arch = "wasm32"))]
use cryptonote::ArchiveSource;
#[cfg(not(target_arch = "wasm32"))]
use cryptonote::crypto::CipherType;
#[cfg(not(target_arch = "wasm32"))]
use cryptonote::{
    create_archive_package_async, extract_archive_package_async, load_archive_async, sanitise_attachment_name,
};
#[cfg(not(target_arch = "wasm32"))]
use std::io::{Read, Write};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::Arc;

#[cfg(not(target_arch = "wasm32"))]
const NOTE: &str = "archive secret body";
#[cfg(not(target_arch = "wasm32"))]
const PASSWORD: &str = "correct horse battery staple";

#[cfg(not(target_arch = "wasm32"))]
fn cancel() -> Arc<std::sync::atomic::AtomicBool> {
    Arc::new(std::sync::atomic::AtomicBool::new(false))
}

#[cfg(not(target_arch = "wasm32"))]
fn encrypted_archive() -> Vec<u8> {
    run_async(async move {
        create_archive_package_async(NOTE, &[], PASSWORD, Some(CipherType::Aes256Gcm), cancel(), |_| {}).await
    })
    .unwrap_or_else(|e| panic!("archive created: {e}"))
}

#[cfg(not(target_arch = "wasm32"))]
fn zip_with_entries(entries: &[(&str, &[u8])]) -> Vec<u8> {
    let mut writer = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
    for (name, data) in entries {
        writer
            .start_file(*name, zip::write::SimpleFileOptions::default())
            .unwrap_or_else(|e| panic!("entry {name}: {e}"));
        writer
            .write_all(data)
            .unwrap_or_else(|e| panic!("entry bytes {name}: {e}"));
    }
    writer.finish().unwrap_or_else(|e| panic!("zip: {e}")).into_inner()
}

#[cfg(not(target_arch = "wasm32"))]
fn with_plain_metadata(payload: &[u8]) -> Vec<u8> {
    zip_with_entries(&[
        (
            "metadata.json",
            br#"{"cipher":null,"kdf":"Argon2id","nonce":[],"salt":[]}"#,
        ),
        ("payload.cpt", payload),
    ])
}

#[test]
fn encrypted_archive_never_contains_the_note_or_the_password() {
    let archive = encrypted_archive();
    assert!(
        !archive.windows(NOTE.len()).any(|window| window == NOTE.as_bytes()),
        "the note body must not appear in the archive bytes"
    );
    assert!(
        !archive
            .windows(PASSWORD.len())
            .any(|window| window == PASSWORD.as_bytes()),
        "the password must not appear in the archive bytes"
    );
}

#[test]
fn plain_metadata_over_an_encrypted_payload_is_refused() {
    let encrypted = encrypted_archive();
    let payload = read_payload(&encrypted);
    let forged = with_plain_metadata(&payload);

    let opened = run_async(load_archive_async(ArchiveSource::Bytes(forged), cancel(), |_| {}));
    assert!(
        opened.is_err(),
        "an archive that claims to be plain but carries an encrypted payload must not open"
    );
}

#[test]
fn plain_metadata_over_an_encrypted_payload_never_yields_the_note() {
    let encrypted = encrypted_archive();
    let payload = read_payload(&encrypted);
    let forged = with_plain_metadata(&payload);

    let extracted = run_async(extract_archive_package_async(
        ArchiveSource::Bytes(forged),
        "",
        cancel(),
        |_| {},
    ));
    match extracted {
        Err(_) => {}
        Ok((note, _)) => assert!(
            !note.contains(NOTE),
            "a forged plain archive must never reveal the note body"
        ),
    }
}

#[test]
fn attachment_names_are_sanitised_against_path_traversal() {
    for hostile in [
        "../../etc/passwd",
        "..\\..\\windows\\system32\\config",
        "nested/../../escape.txt",
        "/absolute/path.txt",
        "..",
        ".",
        "",
    ] {
        let name = sanitise_attachment_name(hostile);
        assert!(
            !name.contains('/') && !name.contains('\\'),
            "an attachment name must not keep a path separator: {hostile} -> {name}"
        );
        assert!(
            !name.starts_with('.'),
            "an attachment name must not start with a dot: {hostile} -> {name}"
        );
        assert!(!name.is_empty(), "an attachment name must never be empty: {hostile}");
    }
}

#[test]
fn traversal_attachment_names_are_sanitised_when_reading_an_archive() {
    let inner = zip_with_entries(&[
        ("note.txt", NOTE.as_bytes()),
        ("attachments/../../escape.txt", b"escaped"),
    ]);
    let package = with_plain_metadata(&inner);
    let (note, files) = run_async(extract_archive_package_async(
        ArchiveSource::Bytes(package),
        "",
        cancel(),
        |_| {},
    ))
    .unwrap_or_else(|e| panic!("archive extracted: {e}"));
    assert_eq!(note, NOTE);
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].name, "escape.txt");
}

#[test]
fn attachment_names_are_sanitised_when_creating_an_archive() {
    let attachment = cryptonote::Attachment {
        name: "../../escape.txt".to_owned(),
        data: Arc::from(b"payload" as &[u8]),
    };
    let package = run_async(async move {
        create_archive_package_async(
            NOTE,
            &[attachment],
            PASSWORD,
            Some(CipherType::Aes256Gcm),
            cancel(),
            |_| {},
        )
        .await
    })
    .unwrap_or_else(|e| panic!("archive created: {e}"));
    let (note, files) = run_async(extract_archive_package_async(
        ArchiveSource::Bytes(package),
        PASSWORD,
        cancel(),
        |_| {},
    ))
    .unwrap_or_else(|e| panic!("archive extracted: {e}"));
    assert_eq!(note, NOTE);
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].name, "escape.txt");
}

fn read_payload(archive: &[u8]) -> Vec<u8> {
    let mut reader =
        zip::ZipArchive::new(std::io::Cursor::new(archive)).unwrap_or_else(|e| panic!("archive opened: {e}"));
    let mut entry = reader
        .by_name("payload.cpt")
        .unwrap_or_else(|e| panic!("payload entry: {e}"));
    let mut payload = Vec::new();
    let _ = entry
        .read_to_end(&mut payload)
        .unwrap_or_else(|e| panic!("payload bytes: {e}"));
    payload
}
