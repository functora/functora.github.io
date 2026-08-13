use dioxus::core::ScopeId;
use dioxus::prelude::{Signal, VirtualDom, rsx};
use dioxus_signals::ReadableExt;
use functora_dioxus::crypto::CipherType;
use functora_dioxus::files::Attachment;
use functora_dioxus::package::{
    ArchiveMetadata, ArchiveSource, PackageStages, aad, extract_package_async, package_async, read_metadata,
};
use functora_dioxus::zip::{create_zip_async, unzip_async};

const AAD_PREFIX: &[u8] = b"test-app.v1";

#[derive(Clone, Copy, PartialEq, Debug)]
enum Stage {
    Zip,
    Encrypt,
    Decrypt,
    Unzip,
}

fn fast_kdf() {
    if std::env::var("FUNCTORA_KDF_M_COST_KIB").is_err() {
        unsafe {
            std::env::set_var("FUNCTORA_KDF_M_COST_KIB", "1024");
            std::env::set_var("FUNCTORA_KDF_T_COST", "1");
        }
    }
}

fn with_runtime<R>(body: impl FnOnce() -> R) -> R {
    fast_kdf();
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    dom.rebuild_in_place();
    dom.in_runtime(body)
}

fn progress() -> Signal<Option<functora_dioxus::progress::Job<Stage>>> {
    Signal::new_in_scope(None, ScopeId(0))
}

fn block_on<R>(fut: impl std::future::Future<Output = R>) -> R {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap_or_else(|e| panic!("tokio runtime: {e}"))
        .block_on(fut)
}

#[test]
fn zip_roundtrip_preserves_entries() {
    with_runtime(|| {
        block_on(async {
            let files = vec![
                Attachment {
                    name: "a.txt".into(),
                    data: b"hello".to_vec().into(),
                },
                Attachment {
                    name: "dir/b.bin".into(),
                    data: vec![1, 2, 3].into(),
                },
            ];
            let zip = create_zip_async(&files, progress(), Stage::Zip)
                .await
                .unwrap_or_else(|e| panic!("zip: {e:?}"));
            let entries = unzip_async(zip, progress(), Stage::Unzip)
                .await
                .unwrap_or_else(|e| panic!("unzip: {e:?}"));
            assert_eq!(entries.len(), 2);
            assert_eq!(entries[0].0, "a.txt");
            assert_eq!(entries[0].1, b"hello");
            assert_eq!(entries[1].0, "dir/b.bin");
            assert_eq!(entries[1].1, vec![1, 2, 3]);
        })
    });
}

#[test]
fn zip_roundtrip_large_payload() {
    with_runtime(|| {
        block_on(async {
            let big = vec![7u8; 3_000_000];
            let zip = create_zip_async(
                &[Attachment {
                    name: "big.bin".into(),
                    data: big.clone().into(),
                }],
                progress(),
                Stage::Zip,
            )
            .await
            .unwrap_or_else(|e| panic!("zip: {e:?}"));
            let entries = unzip_async(zip, progress(), Stage::Unzip)
                .await
                .unwrap_or_else(|e| panic!("unzip: {e:?}"));
            assert_eq!(entries[0].1, big);
        })
    });
}

#[test]
fn zip_progress_reaches_total() {
    with_runtime(|| {
        block_on(async {
            let files = vec![Attachment {
                name: "big.bin".into(),
                data: vec![0u8; 5 * 1024 * 1024].into(),
            }];
            let progress = progress();
            let zip = create_zip_async(&files, progress, Stage::Zip)
                .await
                .unwrap_or_else(|e| panic!("zip: {e:?}"));
            let job = progress
                .try_read()
                .unwrap_or_else(|e| panic!("job reported: {e:?}"))
                .clone()
                .unwrap_or_else(|| panic!("job present"));
            assert_eq!(job.done, job.total);
            assert!(job.total > 0);
            assert!(!zip.is_empty());
        })
    });
}

#[test]
fn package_roundtrip_encrypted() {
    with_runtime(|| {
        block_on(async {
            let entries = vec![
                ("note.txt".to_string(), b"secret note".to_vec()),
                ("attachments/data.bin".to_string(), vec![9, 9, 9]),
            ];
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(
                entries.clone(),
                "pw",
                Some(CipherType::Aes256Gcm),
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let inner = extract_package_async(ArchiveSource::Bytes(pkg), "pw", AAD_PREFIX, stages, progress())
                .await
                .unwrap_or_else(|e| panic!("extract: {e:?}"));
            let unzipped = unzip_async(inner, progress(), Stage::Unzip)
                .await
                .unwrap_or_else(|e| panic!("unzip: {e:?}"));
            assert_eq!(unzipped, entries);
        })
    });
}

#[test]
fn package_roundtrip_plain() {
    with_runtime(|| {
        block_on(async {
            let entries = vec![("note.txt".to_string(), b"plain note".to_vec())];
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(entries.clone(), "", None, AAD_PREFIX, stages, progress())
                .await
                .unwrap_or_else(|e| panic!("package: {e:?}"));
            let inner = extract_package_async(ArchiveSource::Bytes(pkg), "", AAD_PREFIX, stages, progress())
                .await
                .unwrap_or_else(|e| panic!("extract: {e:?}"));
            let unzipped = unzip_async(inner, progress(), Stage::Unzip)
                .await
                .unwrap_or_else(|e| panic!("unzip: {e:?}"));
            assert_eq!(unzipped, entries);
        })
    });
}

#[test]
fn package_metadata_read() {
    with_runtime(|| {
        block_on(async {
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(
                vec![("note.txt".to_string(), b"x".to_vec())],
                "pw",
                Some(CipherType::ChaCha20Poly1305),
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let meta = read_metadata(&ArchiveSource::Bytes(pkg)).unwrap_or_else(|e| panic!("metadata: {e:?}"));
            assert_eq!(meta.cipher, Some(CipherType::ChaCha20Poly1305));
            assert!(!meta.nonce.is_empty());
            assert!(!meta.salt.is_empty());
            let plain = package_async(
                vec![("note.txt".to_string(), b"x".to_vec())],
                "",
                None,
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let plain_meta = read_metadata(&ArchiveSource::Bytes(plain)).unwrap_or_else(|e| panic!("metadata: {e:?}"));
            assert_eq!(
                plain_meta,
                ArchiveMetadata {
                    cipher: None,
                    kdf: functora_dioxus::crypto::Kdf::Argon2id,
                    nonce: Vec::new(),
                    salt: Vec::new(),
                }
            );
        })
    });
}

#[test]
fn package_wrong_password_fails() {
    with_runtime(|| {
        block_on(async {
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(
                vec![("note.txt".to_string(), b"x".to_vec())],
                "correct",
                Some(CipherType::Aes256Gcm),
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let res = extract_package_async(ArchiveSource::Bytes(pkg), "wrong", AAD_PREFIX, stages, progress()).await;
            assert!(res.is_err());
        })
    });
}

#[test]
fn package_wrong_prefix_fails() {
    with_runtime(|| {
        block_on(async {
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(
                vec![("note.txt".to_string(), b"x".to_vec())],
                "pw",
                Some(CipherType::Aes256Gcm),
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let res = extract_package_async(ArchiveSource::Bytes(pkg), "pw", b"other-app.v1", stages, progress()).await;
            assert!(res.is_err());
        })
    });
}

#[test]
fn package_path_roundtrip() {
    with_runtime(|| {
        block_on(async {
            let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
            let pkg = package_async(
                vec![("note.txt".to_string(), b"path note".to_vec())],
                "pw",
                Some(CipherType::Aes256Gcm),
                AAD_PREFIX,
                stages,
                progress(),
            )
            .await
            .unwrap_or_else(|e| panic!("package: {e:?}"));
            let dir = tempfile::tempdir().unwrap_or_else(|e| panic!("tempdir: {e:?}"));
            let path = dir.path().join("test.cryptonote");
            std::fs::write(&path, &pkg).unwrap_or_else(|e| panic!("write: {e:?}"));
            let inner = extract_package_async(ArchiveSource::Path(path), "pw", AAD_PREFIX, stages, progress())
                .await
                .unwrap_or_else(|e| panic!("extract path: {e:?}"));
            let unzipped = unzip_async(inner, progress(), Stage::Unzip)
                .await
                .unwrap_or_else(|e| panic!("unzip: {e:?}"));
            assert_eq!(unzipped, vec![("note.txt".to_string(), b"path note".to_vec())]);
        })
    });
}

#[test]
fn aad_builds_prefix_with_cipher_and_kdf() {
    let kdf = functora_dioxus::crypto::Kdf::Argon2id;
    assert_eq!(
        aad(AAD_PREFIX, CipherType::Aes256Gcm, kdf),
        vec![
            b't',
            b'e',
            b's',
            b't',
            b'-',
            b'a',
            b'p',
            b'p',
            b'.',
            b'v',
            b'1',
            CipherType::Aes256Gcm as u8,
            kdf as u8,
        ]
    );
}
