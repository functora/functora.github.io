#![cfg(all(feature = "crypto", feature = "zip", feature = "package"))]

use functora_core::Error;
use functora_core::crypto::{
    CipherType, EncryptedNote, Kdf, derive_key, stream_decrypt_symmetric, stream_encrypt_symmetric,
};
use functora_core::package::{
    ArchiveMetadata, ArchiveSource, ENCRYPTED_PAYLOAD_FRAME, METADATA_ENTRY, PAYLOAD_ENTRY,
    PLAIN_PAYLOAD_FRAME, PackageStages, PayloadBranch, extract_package_report, package_report,
    read_header, read_metadata,
};
use functora_core::progress::Stage;
use functora_core::zip::{
    MAX_ARCHIVE_ENTRIES, MAX_ARCHIVE_ENTRY_SIZE, MAX_ARCHIVE_TOTAL_SIZE, unzip_report,
};
use std::future::Future;
use std::io::{Cursor, Write};
use std::sync::{Arc, OnceLock};
use std::task::{Context, Poll, Wake, Waker};
use zeroize::Zeroizing;

const AAD: &[u8] = b"functora-core.security.v1";
const PREFIX: &[u8] = b"functora-core.security";

fn block_on<F: Future>(future: F) -> F::Output {
    struct ThreadWaker(std::thread::Thread);

    impl Wake for ThreadWaker {
        fn wake(self: Arc<Self>) {
            self.0.unpark();
        }

        fn wake_by_ref(self: &Arc<Self>) {
            self.0.unpark();
        }
    }

    let mut pinned = Box::pin(future);
    let waker = Waker::from(Arc::new(ThreadWaker(std::thread::current())));
    let mut context = Context::from_waker(&waker);
    loop {
        match pinned.as_mut().poll(&mut context) {
            Poll::Ready(value) => return value,
            Poll::Pending => std::thread::yield_now(),
        }
    }
}

fn reporter() -> functora_core::worker::Reporter<Stage> {
    Box::new(|_job| {})
}

fn stages() -> PackageStages<Stage> {
    PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt)
}

fn sealed_note() -> EncryptedNote {
    static SEALED: OnceLock<EncryptedNote> = OnceLock::new();
    SEALED
        .get_or_init(|| {
            stream_encrypt_symmetric(b"top secret payload", "pw", CipherType::Aes256Gcm, AAD)
                .unwrap_or_else(|e| panic!("encrypt: {e:?}"))
        })
        .clone()
}

fn zip_bytes(entries: &[(String, Vec<u8>)]) -> Vec<u8> {
    let mut buf = Vec::new();
    {
        let mut zip = zip::ZipWriter::new(Cursor::new(&mut buf));
        let options = zip::write::FileOptions::<'static, 'static, ()>::default()
            .compression_method(zip::CompressionMethod::Stored);
        for (name, data) in entries {
            zip.start_file(name.as_str(), options)
                .unwrap_or_else(|e| panic!("start_file: {e:?}"));
            zip.write_all(data)
                .unwrap_or_else(|e| panic!("write_all: {e:?}"));
        }
        let _ = zip.finish().unwrap_or_else(|e| panic!("finish: {e:?}"));
    }
    buf
}

fn find_all(haystack: &[u8], needle: &[u8]) -> Vec<usize> {
    haystack
        .windows(needle.len())
        .enumerate()
        .filter_map(|(index, window)| (window == needle).then_some(index))
        .collect()
}

fn patch_declared_sizes(archive: &mut [u8], sizes: &[u64]) {
    let locals = find_all(archive, &[0x50, 0x4b, 0x03, 0x04]);
    let centrals = find_all(archive, &[0x50, 0x4b, 0x01, 0x02]);
    assert_eq!(locals.len(), sizes.len(), "local header count");
    assert_eq!(centrals.len(), sizes.len(), "central header count");
    for (offset, size) in locals.iter().zip(sizes) {
        let size32 = u32::try_from(*size).unwrap_or_else(|e| panic!("size fits u32: {e:?}"));
        archive[offset + 22..offset + 26].copy_from_slice(&size32.to_le_bytes());
    }
    for (offset, size) in centrals.iter().zip(sizes) {
        let size32 = u32::try_from(*size).unwrap_or_else(|e| panic!("size fits u32: {e:?}"));
        archive[offset + 24..offset + 28].copy_from_slice(&size32.to_le_bytes());
    }
}

fn encrypted_metadata() -> ArchiveMetadata {
    ArchiveMetadata {
        cipher: Some(CipherType::Aes256Gcm),
        kdf: Kdf::Argon2id,
        nonce: vec![0u8; 7],
        salt: vec![0u8; 32],
    }
}

fn plain_metadata() -> ArchiveMetadata {
    ArchiveMetadata {
        cipher: None,
        kdf: Kdf::Argon2id,
        nonce: Vec::new(),
        salt: Vec::new(),
    }
}

fn metadata_bytes(meta: &ArchiveMetadata) -> Vec<u8> {
    serde_json::to_vec(meta).unwrap_or_else(|e| panic!("json: {e:?}"))
}

fn archive_with(meta: &ArchiveMetadata, payload: &[u8]) -> Vec<u8> {
    zip_bytes(&[
        (METADATA_ENTRY.to_string(), metadata_bytes(meta)),
        (PAYLOAD_ENTRY.to_string(), payload.to_vec()),
    ])
}

#[test]
fn stream_decrypt_rejects_an_empty_ciphertext() {
    let mut note = sealed_note();
    note.ciphertext.clear();
    assert!(
        stream_decrypt_symmetric(&note, "pw", AAD).is_err(),
        "an empty ciphertext must not authenticate as an empty plaintext"
    );
}

#[test]
fn stream_decrypt_rejects_a_truncated_ciphertext() {
    let mut note = sealed_note();
    let _ = note.ciphertext.pop();
    assert!(stream_decrypt_symmetric(&note, "pw", AAD).is_err());
}

#[test]
fn stream_decrypt_rejects_an_extended_ciphertext() {
    let mut note = sealed_note();
    note.ciphertext.extend_from_slice(&[0u8; 1]);
    assert!(stream_decrypt_symmetric(&note, "pw", AAD).is_err());
}

#[test]
fn stream_decrypt_rejects_a_tampered_salt_or_nonce() {
    let mut salt = sealed_note();
    salt.salt[0] ^= 0xFF;
    assert!(stream_decrypt_symmetric(&salt, "pw", AAD).is_err());

    let mut nonce = sealed_note();
    nonce.nonce[0] ^= 0xFF;
    assert!(stream_decrypt_symmetric(&nonce, "pw", AAD).is_err());
}

#[test]
fn stream_encrypt_roundtrips_an_empty_plaintext() {
    let note = stream_encrypt_symmetric(b"", "pw", CipherType::ChaCha20Poly1305, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert!(!note.ciphertext.is_empty(), "the tag must still be present");
    assert_eq!(
        Vec::<u8>::new(),
        stream_decrypt_symmetric(&note, "pw", AAD).unwrap_or_else(|e| panic!("decrypt: {e:?}"))
    );
}

#[test]
fn package_decrypt_rejects_an_empty_payload() {
    let archive = archive_with(&encrypted_metadata(), ENCRYPTED_PAYLOAD_FRAME);
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "pw",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::Decrypt(_))),
        "an empty payload must not unseal, got {result:?}"
    );
}

#[test]
fn extract_package_rejects_a_metadata_that_drops_the_cipher() {
    let payload = [ENCRYPTED_PAYLOAD_FRAME, b"ciphertext".as_slice()].concat();
    let archive = archive_with(&plain_metadata(), &payload);
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::InvalidFormat(_))),
        "a metadata file that drops the cipher must not move an encrypted archive into the plain branch, got {result:?}"
    );
}

#[test]
fn extract_package_rejects_a_metadata_that_adds_a_cipher() {
    let payload = [PLAIN_PAYLOAD_FRAME, b"plain body".as_slice()].concat();
    let archive = archive_with(&encrypted_metadata(), &payload);
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "pw",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::InvalidFormat(_))),
        "a metadata file that adds a cipher must not move a plain archive into the encrypted branch, got {result:?}"
    );
}

#[test]
fn extract_package_rejects_a_payload_without_a_branch_frame() {
    let archive = archive_with(
        &plain_metadata(),
        b"a payload that carries no branch frame at all",
    );
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::InvalidFormat(_))),
        "a payload without a branch frame must be refused, got {result:?}"
    );
}

#[test]
fn extract_package_rejects_a_truncated_payload() {
    let archive = archive_with(&plain_metadata(), b"short");
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        result.is_err(),
        "a payload that is shorter than the branch frame must be refused, got {result:?}"
    );
}

#[test]
fn package_roundtrips_a_plain_archive() {
    let entries = vec![("note.txt".to_string(), Arc::from(b"plain body".to_vec()))];
    let mut report = reporter();
    let archive = block_on(package_report(
        entries,
        "",
        None,
        PREFIX,
        stages(),
        &mut report,
        None,
    ))
    .unwrap_or_else(|e| panic!("package: {e:?}"));
    let mut extract_report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "",
        PREFIX,
        stages(),
        &mut extract_report,
        None,
    ));
    assert!(
        result.is_ok(),
        "a plain archive must still open, got {result:?}"
    );
}

#[test]
fn read_header_reports_the_branch_of_an_encrypted_archive() {
    let entries = vec![("note.txt".to_string(), Arc::from(b"secret".to_vec()))];
    let mut report = reporter();
    let archive = block_on(package_report(
        entries,
        "pw",
        Some(CipherType::Aes256Gcm),
        PREFIX,
        stages(),
        &mut report,
        None,
    ))
    .unwrap_or_else(|e| panic!("package: {e:?}"));
    let header =
        read_header(&ArchiveSource::Bytes(archive)).unwrap_or_else(|e| panic!("header: {e:?}"));
    assert_eq!(header.branch, PayloadBranch::Encrypted);
    assert_eq!(header.metadata.cipher, Some(CipherType::Aes256Gcm));
}

#[test]
fn read_header_reports_the_branch_of_a_plain_archive() {
    let entries = vec![("note.txt".to_string(), Arc::from(b"plain".to_vec()))];
    let mut report = reporter();
    let archive = block_on(package_report(
        entries,
        "",
        None,
        PREFIX,
        stages(),
        &mut report,
        None,
    ))
    .unwrap_or_else(|e| panic!("package: {e:?}"));
    let header =
        read_header(&ArchiveSource::Bytes(archive)).unwrap_or_else(|e| panic!("header: {e:?}"));
    assert_eq!(header.branch, PayloadBranch::Plain);
    assert_eq!(header.metadata.cipher, None);
}

#[test]
fn read_header_rejects_a_metadata_that_does_not_match_the_payload() {
    let payload = [ENCRYPTED_PAYLOAD_FRAME, b"ciphertext".as_slice()].concat();
    let archive = archive_with(&plain_metadata(), &payload);
    let result = read_header(&ArchiveSource::Bytes(archive));
    assert!(
        matches!(result, Err(Error::InvalidFormat(_))),
        "a metadata file that does not match the payload branch must be refused, got {result:?}"
    );
}

#[test]
fn extract_package_refuses_an_empty_password() {
    let entries = vec![("note.txt".to_string(), Arc::from(b"secret".to_vec()))];
    let mut report = reporter();
    let archive = block_on(package_report(
        entries,
        "pw",
        Some(CipherType::Aes256Gcm),
        PREFIX,
        stages(),
        &mut report,
        None,
    ))
    .unwrap_or_else(|e| panic!("package: {e:?}"));
    let mut extract_report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "",
        PREFIX,
        stages(),
        &mut extract_report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::PasswordRequired)),
        "an encrypted archive must not open without a password, got {result:?}"
    );
}

#[test]
fn package_rejects_an_oversized_declared_entry() {
    let meta = metadata_bytes(&plain_metadata());
    let mut archive = zip_bytes(&[
        (METADATA_ENTRY.to_string(), meta.clone()),
        (
            PAYLOAD_ENTRY.to_string(),
            [PLAIN_PAYLOAD_FRAME, b"small".as_slice()].concat(),
        ),
    ]);
    patch_declared_sizes(
        &mut archive,
        &[meta.len() as u64, MAX_ARCHIVE_ENTRY_SIZE + 1],
    );
    let mut report = reporter();
    let result = block_on(extract_package_report(
        ArchiveSource::Bytes(archive),
        "pw",
        PREFIX,
        stages(),
        &mut report,
        None,
    ));
    assert!(
        matches!(result, Err(Error::FileTooLarge { .. })),
        "an oversized declared entry must be refused before allocation, got {result:?}"
    );
}

#[test]
fn package_rejects_an_oversized_metadata_entry() {
    let meta = metadata_bytes(&encrypted_metadata());
    let mut archive = zip_bytes(&[(METADATA_ENTRY.to_string(), meta)]);
    patch_declared_sizes(&mut archive, &[MAX_ARCHIVE_ENTRY_SIZE + 1]);
    let result = read_metadata(&ArchiveSource::Bytes(archive));
    assert!(
        matches!(result, Err(Error::FileTooLarge { .. })),
        "an oversized metadata entry must be refused before allocation, got {result:?}"
    );
}

#[test]
fn unzip_rejects_too_many_entries() {
    let entries: Vec<(String, Vec<u8>)> = (0..=MAX_ARCHIVE_ENTRIES)
        .map(|index| (format!("entry-{index}"), Vec::new()))
        .collect();
    let archive = zip_bytes(&entries);
    let mut report = reporter();
    let result = block_on(unzip_report(archive, Stage::Unzip, &mut report, None));
    assert!(
        matches!(result, Err(Error::TooManyEntries { .. })),
        "an archive with too many entries must be refused, got {result:?}"
    );
}

#[test]
fn unzip_rejects_an_oversized_total_size() {
    let entries: Vec<(String, Vec<u8>)> = (0..3)
        .map(|index| (format!("entry-{index}"), b"x".to_vec()))
        .collect();
    let mut archive = zip_bytes(&entries);
    patch_declared_sizes(&mut archive, &[MAX_ARCHIVE_ENTRY_SIZE; 3]);
    let mut report = reporter();
    let result = block_on(unzip_report(archive, Stage::Unzip, &mut report, None));
    assert!(
        matches!(result, Err(Error::FileTooLarge { .. })),
        "an archive above the total size budget must be refused, got {result:?}"
    );
}

#[test]
fn unzip_accepts_an_archive_inside_the_budget() {
    let entries = vec![("note.txt".to_string(), b"hello".to_vec())];
    let archive = zip_bytes(&entries);
    let mut report = reporter();
    let result = block_on(unzip_report(archive, Stage::Unzip, &mut report, None));
    assert_eq!(Ok(entries), result, "an ordinary archive must still unzip");
}

#[test]
fn derive_key_returns_a_zeroizing_buffer() {
    let salt = vec![3u8; 32];
    let key: Zeroizing<Vec<u8>> =
        derive_key("pw", &salt, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    assert_eq!(key.len(), 32);
}

#[test]
fn package_budget_constants_are_ordered() {
    const { assert!(MAX_ARCHIVE_ENTRY_SIZE <= MAX_ARCHIVE_TOTAL_SIZE) };
    const { assert!(MAX_ARCHIVE_ENTRIES > 0) };
}
