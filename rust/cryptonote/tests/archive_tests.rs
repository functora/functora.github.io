use cryptonote::archive::{
    create_archive_package_async, extract_archive_package_async, read_archive_metadata, ArchiveMetadata, ArchiveSource,
    Attachment,
};
use cryptonote::crypto::{CipherType, Kdf};
use cryptonote::progress::Stage;
use cryptonote::AppError;
use std::io::Read;

mod common;

async fn roundtrip(
    note: &str,
    attachments: &[Attachment],
    password: &str,
    cipher: Option<CipherType>,
) -> Result<(String, Vec<Attachment>), AppError> {
    let progress = common::progress();
    let pkg = create_archive_package_async(note, attachments, password, cipher, progress).await?;
    extract_archive_package_async(ArchiveSource::Bytes(pkg), password, progress).await
}

#[test]
fn test_archive_roundtrip_chacha20() {
    common::with_runtime(|| {
        common::block_on(async {
            let note = "Hello, this is my secret note!";
            let attachments = vec![
                Attachment {
                    name: "hello.txt".into(),
                    data: b"Hello, World!".to_vec(),
                },
                Attachment {
                    name: "data.bin".into(),
                    data: vec![1, 2, 3, 4, 5],
                },
            ];
            let (text, files) = roundtrip(note, &attachments, "password", Some(CipherType::ChaCha20Poly1305))
                .await
                .unwrap();
            assert_eq!(text, note);
            assert_eq!(files.len(), 2);
            assert_eq!(files[0].name, "hello.txt");
            assert_eq!(files[0].data, b"Hello, World!");
            assert_eq!(files[1].name, "data.bin");
            assert_eq!(files[1].data, vec![1, 2, 3, 4, 5]);
        })
    });
}

#[test]
fn test_archive_roundtrip_aes() {
    common::with_runtime(|| {
        common::block_on(async {
            let attachments = vec![Attachment {
                name: "file.txt".into(),
                data: b"content".to_vec(),
            }];
            let (text, files) = roundtrip("AES note", &attachments, "strong_pw", Some(CipherType::Aes256Gcm))
                .await
                .unwrap();
            assert_eq!(text, "AES note");
            assert_eq!(files[0].name, "file.txt");
            assert_eq!(files[0].data, b"content");
        })
    });
}

#[test]
fn test_archive_no_attachments() {
    common::with_runtime(|| {
        common::block_on(async {
            let (text, files) = roundtrip("Just a note", &[], "pw", Some(CipherType::ChaCha20Poly1305))
                .await
                .unwrap();
            assert_eq!(text, "Just a note");
            assert!(files.is_empty());
        })
    });
}

#[test]
fn test_archive_wrong_password() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let pkg = create_archive_package_async("secret", &[], "correct_pw", Some(CipherType::Aes256Gcm), progress)
                .await
                .unwrap();
            assert!(
                extract_archive_package_async(ArchiveSource::Bytes(pkg), "wrong_pw", common::progress())
                    .await
                    .is_err()
            );
        })
    });
}

#[test]
fn test_archive_many_attachments() {
    common::with_runtime(|| {
        common::block_on(async {
            let note = "Note with many files";
            let attachments: Vec<_> = (0..100)
                .map(|i| Attachment {
                    name: format!("file_{}.bin", i),
                    data: vec![i as u8; 100],
                })
                .collect();
            let (text, files) = roundtrip(note, &attachments, "pw", Some(CipherType::ChaCha20Poly1305))
                .await
                .unwrap();
            assert_eq!(text, note);
            assert_eq!(files.len(), 100);
            for (i, f) in files.iter().enumerate() {
                assert_eq!(f.name, format!("file_{}.bin", i));
                assert_eq!(f.data.len(), 100);
            }
        })
    });
}

#[test]
fn archive_metadata_serde_roundtrip() {
    let meta = ArchiveMetadata {
        cipher: Some(CipherType::ChaCha20Poly1305),
        kdf: Kdf::Argon2id,
        nonce: vec![10; 12],
        salt: vec![20; 32],
    };
    let json = serde_json::to_string(&meta).unwrap();
    assert!(!json.contains("version"));
    let back: ArchiveMetadata = serde_json::from_str(&json).unwrap();
    assert_eq!(meta, back);
}

#[test]
fn attachment_default() {
    let att = Attachment::default();
    assert_eq!(att.name, "");
    assert!(att.data.is_empty());
}

#[test]
fn extract_archive_ignores_extra_entries() {
    use std::io::Write;
    use zip::write::FileOptions;
    use zip::CompressionMethod;
    common::with_runtime(|| {
        common::block_on(async {
            let note = "test note";
            let progress = common::progress();
            let pkg = create_archive_package_async(note, &[], "pw", Some(CipherType::ChaCha20Poly1305), progress)
                .await
                .unwrap();
            let stored: FileOptions<'static, ()> = FileOptions::default().compression_method(CompressionMethod::Stored);
            let mut augmented = Vec::new();
            {
                let mut zip = zip::ZipWriter::new(std::io::Cursor::new(&mut augmented));
                let mut reader = zip::ZipArchive::new(std::io::Cursor::new(&pkg)).expect("Read original failed");
                for i in 0..reader.len() {
                    let mut entry = reader.by_index(i).expect("Entry read failed");
                    let name = entry.name().to_string();
                    let mut data = Vec::new();
                    std::io::Read::read_to_end(&mut entry, &mut data).expect("Data read failed");
                    zip.start_file(&name, stored).expect("Start file failed");
                    zip.write_all(&data).expect("Write data failed");
                }
                zip.start_file("extra.txt", stored).expect("Extra start failed");
                zip.write_all(b"extra content").expect("Extra write failed");
                zip.finish().expect("Finish failed");
            }
            let (text, files) =
                extract_archive_package_async(ArchiveSource::Bytes(augmented), "pw", common::progress())
                    .await
                    .expect("extract failed");
            assert_eq!(text, note);
            assert!(files.is_empty());
        })
    });
}

#[test]
fn test_archive_empty_note() {
    common::with_runtime(|| {
        common::block_on(async {
            let (text, files) = roundtrip("", &[], "pw", Some(CipherType::Aes256Gcm)).await.unwrap();
            assert_eq!(text, "");
            assert!(files.is_empty());
        })
    });
}

#[test]
fn test_plaintext_archive_roundtrip() {
    common::with_runtime(|| {
        common::block_on(async {
            let attachments = vec![
                Attachment {
                    name: "photo.png".into(),
                    data: vec![9, 9, 9],
                },
                Attachment {
                    name: "doc.txt".into(),
                    data: b"plain".to_vec(),
                },
            ];
            let (text, files) = roundtrip("Plain note with attachments", &attachments, "", None)
                .await
                .unwrap();
            assert_eq!(text, "Plain note with attachments");
            assert_eq!(files.len(), 2);
            assert_eq!(files[0].data, vec![9, 9, 9]);
            assert_eq!(files[1].data, b"plain");
        })
    });
}

#[test]
fn test_plaintext_archive_metadata_is_none() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let pkg = create_archive_package_async("plain", &[], "", None, progress)
                .await
                .unwrap();
            let meta = read_archive_metadata(&ArchiveSource::Bytes(pkg)).expect("Metadata read failed");
            assert_eq!(meta.cipher, None);
            assert_eq!(meta.kdf, Kdf::Argon2id);
            assert!(meta.nonce.is_empty());
            assert!(meta.salt.is_empty());
        })
    });
}

#[test]
fn archive_metadata_old_format_rejected_without_kdf() {
    let json = r#"{"cipher":"Aes256Gcm","nonce":[1,2,3],"salt":[4,5,6]}"#;
    let result: Result<ArchiveMetadata, _> = serde_json::from_str(json);
    assert!(result.is_err());
}

#[test]
fn archive_wrong_nonce_length_returns_error() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let mut pkg = create_archive_package_async("note", &[], "pw", Some(CipherType::ChaCha20Poly1305), progress)
                .await
                .unwrap();
            let mut archive = zip::ZipArchive::new(std::io::Cursor::new(&pkg)).expect("Read archive failed");
            let mut meta_json = Vec::new();
            archive
                .by_name("metadata.json")
                .expect("Meta missing")
                .read_to_end(&mut meta_json)
                .unwrap();
            let meta: ArchiveMetadata = serde_json::from_slice(&meta_json).unwrap();
            let mut tampered = meta.clone();
            tampered.nonce = vec![1, 2, 3];
            let meta_json = serde_json::to_vec(&tampered).unwrap();
            pkg = rebuild_package(&pkg, &meta_json);
            let result = extract_archive_package_async(ArchiveSource::Bytes(pkg), "pw", common::progress()).await;
            assert!(matches!(result, Err(AppError::InvalidFormat(_))));
        })
    });
}

fn rebuild_package(pkg: &[u8], new_meta: &[u8]) -> Vec<u8> {
    use std::io::Write;
    use zip::write::FileOptions;
    use zip::CompressionMethod;
    let mut out = Vec::new();
    {
        let mut writer = zip::ZipWriter::new(std::io::Cursor::new(&mut out));
        let stored = FileOptions::<'static, ()>::default().compression_method(CompressionMethod::Stored);
        writer.start_file("metadata.json", stored).unwrap();
        writer.write_all(new_meta).unwrap();
        let mut archive = zip::ZipArchive::new(std::io::Cursor::new(pkg)).unwrap();
        let mut payload = Vec::new();
        archive
            .by_name("payload.cpt")
            .unwrap()
            .read_to_end(&mut payload)
            .unwrap();
        writer.start_file("payload.cpt", stored).unwrap();
        writer.write_all(&payload).unwrap();
        writer.finish().unwrap();
    }
    out
}

async fn v2_roundtrip(cipher: Option<CipherType>) {
    let note = "async archive note";
    let attachments = vec![Attachment {
        name: "big.bin".into(),
        data: (0..200_000).map(|i| (i % 251) as u8).collect(),
    }];
    let (text, files) = roundtrip(note, &attachments, "pw", cipher).await.unwrap();
    assert_eq!(text, note);
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].data, attachments[0].data);
}

#[test]
fn test_v2_archive_roundtrip_chacha20() {
    common::with_runtime(|| common::block_on(v2_roundtrip(Some(CipherType::ChaCha20Poly1305))));
}

#[test]
fn test_v2_archive_roundtrip_aes() {
    common::with_runtime(|| common::block_on(v2_roundtrip(Some(CipherType::Aes256Gcm))));
}

#[test]
fn test_v2_plaintext_archive_roundtrip() {
    common::with_runtime(|| common::block_on(v2_roundtrip(None)));
}

#[test]
fn v2_encryption_reports_completed_progress() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let attachments = vec![Attachment {
                name: "spread.bin".into(),
                data: (0..300_000).map(|i| i as u8).collect(),
            }];
            let pkg =
                create_archive_package_async("n", &attachments, "pw", Some(CipherType::ChaCha20Poly1305), progress)
                    .await
                    .expect("encrypt failed");
            assert!(!pkg.is_empty());
            let job = progress().expect("expected progress");
            assert_eq!(job.stage, Stage::Encrypt);
            assert_eq!(job.done, job.total);
            assert!(job.total > 0);
        })
    });
}

#[test]
fn test_v2_archive_wrong_password_fails() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let pkg = create_archive_package_async(
                "n",
                &[Attachment {
                    name: "a.bin".into(),
                    data: vec![9; 4096],
                }],
                "pw",
                Some(CipherType::Aes256Gcm),
                progress,
            )
            .await
            .expect("encrypt failed");
            assert!(
                extract_archive_package_async(ArchiveSource::Bytes(pkg), "bad", common::progress())
                    .await
                    .is_err()
            );
        })
    });
}

fn big_attachment(size: usize) -> Attachment {
    Attachment {
        name: "big.bin".into(),
        data: (0..size)
            .map(|i| ((i.wrapping_mul(31) % 251).wrapping_add(3)) as u8)
            .collect(),
    }
}

async fn roundtrip_large(cipher: Option<CipherType>) {
    let size = 16 * 1024 * 1024;
    let big = big_attachment(size);
    let (text, files) = roundtrip("large note", std::slice::from_ref(&big), "pw", cipher)
        .await
        .unwrap();
    assert_eq!(text, "large note");
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].name, "big.bin");
    assert_eq!(files[0].data, big.data);
    assert_eq!(files[0].data.len(), size);
}

#[test]
fn test_large_attachment_roundtrip_plain() {
    common::with_runtime(|| common::block_on(roundtrip_large(None)));
}

#[test]
fn test_large_attachment_roundtrip_encrypted() {
    common::with_runtime(|| common::block_on(roundtrip_large(Some(CipherType::Aes256Gcm))));
}

#[test]
fn test_archive_zip_progress_advances_to_total() {
    common::with_runtime(|| {
        common::block_on(async {
            let big = big_attachment(4 * 1024 * 1024);
            let total = b"note".len() as u64 + big.data.len() as u64;
            let progress = common::progress();
            let atts = vec![big];
            let create = create_archive_package_async("note", &atts, "", None, progress);
            let sample = async {
                loop {
                    if let Some(job) = progress() {
                        if job.stage == Stage::Zip && job.done >= job.total {
                            return (job.done, job.name.clone());
                        }
                    }
                    tokio::task::yield_now().await;
                }
            };
            let (pkg, (zipped, name)) = tokio::join!(create, sample);
            pkg.expect("archive build failed");
            assert_eq!(zipped, total);
            assert_eq!(name.as_deref(), Some("big.bin"));
        })
    });
}

fn temp_path(pkg: &[u8]) -> std::path::PathBuf {
    use std::io::Write;
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let path = std::env::temp_dir().join(format!(
        "cryptonote-test-{}-{}.cryptonote",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::File::create(&path)
        .expect("create failed")
        .write_all(pkg)
        .expect("write failed");
    path
}

#[test]
fn archive_path_source_extracts_encrypted() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let pkg = create_archive_package_async(
                "path note",
                &[Attachment {
                    name: "a.txt".into(),
                    data: b"path data".to_vec(),
                }],
                "pw",
                Some(CipherType::Aes256Gcm),
                progress,
            )
            .await
            .unwrap();
            let path = temp_path(&pkg);
            let meta = read_archive_metadata(&ArchiveSource::Path(path.clone())).expect("metadata");
            assert_eq!(meta.cipher, Some(CipherType::Aes256Gcm));
            let (text, files) =
                extract_archive_package_async(ArchiveSource::Path(path.clone()), "pw", common::progress())
                    .await
                    .expect("extract");
            assert_eq!(text, "path note");
            assert_eq!(files[0].name, "a.txt");
            assert_eq!(files[0].data, b"path data");
            assert_eq!(ArchiveSource::Path(path.clone()).into_bytes().unwrap(), pkg);
            std::fs::remove_file(path).ok();
        })
    });
}

#[test]
fn archive_path_source_extracts_plaintext() {
    common::with_runtime(|| {
        common::block_on(async {
            let progress = common::progress();
            let pkg = create_archive_package_async("plain path", &[], "", None, progress)
                .await
                .unwrap();
            let path = temp_path(&pkg);
            let (text, files) =
                extract_archive_package_async(ArchiveSource::Path(path.clone()), "", common::progress())
                    .await
                    .expect("extract");
            assert_eq!(text, "plain path");
            assert!(files.is_empty());
            std::fs::remove_file(path).ok();
        })
    });
}

#[test]
fn archive_path_source_missing_file_errors() {
    common::with_runtime(|| {
        common::block_on(async {
            let path = std::env::temp_dir().join("cryptonote-missing.cryptonote");
            assert!(read_archive_metadata(&ArchiveSource::Path(path.clone())).is_err());
            assert!(
                extract_archive_package_async(ArchiveSource::Path(path), "pw", common::progress())
                    .await
                    .is_err()
            );
        })
    });
}
