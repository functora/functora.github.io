use cryptonote::archive::{create_archive_package, extract_archive_package, read_archive_metadata, Attachment};
use cryptonote::crypto::CipherType;

#[test]
fn test_archive_roundtrip_chacha20() {
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
    let pkg = create_archive_package(note, &attachments, "password", CipherType::ChaCha20Poly1305)
        .expect("Package creation failed");

    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files.len(), 3);
    assert_eq!(meta.files[0].name, "_note.txt");
    assert_eq!(meta.files[1].name, "hello.txt");
    assert_eq!(meta.files[2].name, "data.bin");

    let files = extract_archive_package(&pkg, "password").expect("Package extraction failed");
    assert_eq!(files.len(), 3);
    let note_file = files.iter().find(|f| f.name == "_note.txt").unwrap();
    assert_eq!(String::from_utf8(note_file.data.clone()).unwrap(), note);
    let hello = files.iter().find(|f| f.name == "hello.txt").unwrap();
    assert_eq!(hello.data, b"Hello, World!");
    let data = files.iter().find(|f| f.name == "data.bin").unwrap();
    assert_eq!(data.data, vec![1, 2, 3, 4, 5]);
}

#[test]
fn test_archive_roundtrip_aes() {
    let note = "AES note";
    let attachments = vec![Attachment {
        name: "file.txt".into(),
        data: b"content".to_vec(),
    }];
    let pkg = create_archive_package(note, &attachments, "strong_pw", CipherType::Aes256Gcm)
        .expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files.len(), 2);
    assert_eq!(meta.files[0].name, "_note.txt");

    let files = extract_archive_package(&pkg, "strong_pw").expect("Package extraction failed");
    assert_eq!(files.len(), 2);
    assert_eq!(String::from_utf8(files[0].data.clone()).unwrap(), "AES note");
    assert_eq!(files[1].data, b"content");
}

#[test]
fn test_archive_no_attachments() {
    let pkg = create_archive_package("Just a note", &[], "pw", CipherType::ChaCha20Poly1305)
        .expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files.len(), 1);
    assert_eq!(meta.files[0].name, "_note.txt");
    let files = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(files.len(), 1);
    assert_eq!(String::from_utf8(files[0].data.clone()).unwrap(), "Just a note");
}

#[test]
fn test_archive_wrong_password() {
    let pkg = create_archive_package("secret", &[], "correct", CipherType::ChaCha20Poly1305)
        .expect("Package creation failed");
    let result = extract_archive_package(&pkg, "wrong");
    assert!(result.is_err());
}

#[test]
fn test_archive_empty_note() {
    let pkg = create_archive_package("", &[], "pw", CipherType::Aes256Gcm).expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files[0].size, 0);
    let files = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(String::from_utf8(files[0].data.clone()).unwrap(), "");
}

#[test]
fn test_archive_unicode_filenames() {
    let attachments = vec![Attachment {
        name: "привет.txt".into(),
        data: b"hello".to_vec(),
    }];
    let pkg = create_archive_package("note", &attachments, "pw", CipherType::ChaCha20Poly1305)
        .expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files[1].name, "привет.txt");
    let files = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    let f = files.iter().find(|f| f.name == "привет.txt").unwrap();
    assert_eq!(f.data, b"hello");
}

#[test]
fn test_archive_multiple_attachments() {
    let attachments: Vec<Attachment> = (0..10)
        .map(|i| Attachment {
            name: format!("file_{}.bin", i),
            data: vec![i as u8; 100],
        })
        .collect();
    let pkg =
        create_archive_package("multi", &attachments, "pw", CipherType::Aes256Gcm).expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files.len(), 11);
    let files = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(files.len(), 11);
    for i in 0..10 {
        let f = files.iter().find(|f| f.name == format!("file_{}.bin", i)).unwrap();
        assert_eq!(f.data, vec![i as u8; 100]);
    }
}

#[test]
fn test_metadata_readable_without_password() {
    let pkg = create_archive_package(
        "secret text",
        &[Attachment {
            name: "confidential.doc".into(),
            data: vec![0; 999],
        }],
        "hunter2",
        CipherType::ChaCha20Poly1305,
    )
    .expect("Package creation failed");
    let meta = read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.files[0].name, "_note.txt");
    assert_eq!(meta.files[0].size, 11);
    assert_eq!(meta.files[1].name, "confidential.doc");
    assert_eq!(meta.files[1].size, 999);
    assert!(extract_archive_package(&pkg, "hunter2").is_ok());
    assert!(extract_archive_package(&pkg, "wrong").is_err());
}

#[test]
fn test_archive_different_ciphers_different_packages() {
    let note = "same note";
    let atts = [Attachment {
        name: "f.txt".into(),
        data: b"data".to_vec(),
    }];
    let p1 = create_archive_package(note, &atts, "pw", CipherType::ChaCha20Poly1305).expect("Package creation failed");
    let p2 = create_archive_package(note, &atts, "pw", CipherType::Aes256Gcm).expect("Package creation failed");
    assert_ne!(p1, p2);
}

#[test]
fn create_zip_empty_list() {
    use cryptonote::archive::create_zip;
    let zip = create_zip(&[]).expect("create_zip failed");
    assert!(!zip.is_empty());
    let archive = zip::ZipArchive::new(std::io::Cursor::new(zip)).expect("ZIP parse failed");
    assert_eq!(archive.len(), 0);
}

#[test]
fn create_zip_single_file() {
    use cryptonote::archive::{create_zip, Attachment};
    let zip = create_zip(&[Attachment {
        name: "test.txt".into(),
        data: b"content".to_vec(),
    }])
    .expect("create_zip failed");
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(zip)).expect("ZIP parse failed");
    assert_eq!(archive.len(), 1);
    let mut file = archive.by_index(0).unwrap();
    assert_eq!(file.name(), "test.txt");
    let mut data = Vec::new();
    std::io::Read::read_to_end(&mut file, &mut data).unwrap();
    assert_eq!(data, b"content");
}

#[test]
fn create_zip_multiple_files() {
    use cryptonote::archive::{create_zip, Attachment};
    let files = vec![
        Attachment {
            name: "a.bin".into(),
            data: vec![1, 2, 3],
        },
        Attachment {
            name: "b.bin".into(),
            data: vec![4, 5],
        },
    ];
    let zip = create_zip(&files).expect("create_zip failed");
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(zip)).expect("ZIP parse failed");
    assert_eq!(archive.len(), 2);
    let names: Vec<String> = (0..archive.len())
        .map(|i| archive.by_index(i).unwrap().name().to_string())
        .collect();
    assert!(names.contains(&"a.bin".to_string()));
    assert!(names.contains(&"b.bin".to_string()));
}

#[test]
fn create_zip_binary_data_roundtrip() {
    use cryptonote::archive::{create_zip, Attachment};
    let data: Vec<u8> = (0..255).collect();
    let zip = create_zip(&[Attachment {
        name: "data.bin".into(),
        data: data.clone(),
    }])
    .expect("create_zip failed");
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(zip)).expect("ZIP parse failed");
    let mut out = Vec::new();
    std::io::Read::read_to_end(&mut archive.by_index(0).unwrap(), &mut out).unwrap();
    assert_eq!(out, data);
}

#[test]
fn create_zip_unicode_filename() {
    use cryptonote::archive::{create_zip, Attachment};
    let zip = create_zip(&[Attachment {
        name: "привет.txt".into(),
        data: b"test".to_vec(),
    }])
    .expect("create_zip failed");
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(zip)).expect("ZIP parse failed");
    assert_eq!(archive.by_index(0).unwrap().name(), "привет.txt");
}

#[test]
fn ciphertype_serde_roundtrip() {
    use cryptonote::crypto::CipherType;
    for variant in &[CipherType::ChaCha20Poly1305, CipherType::Aes256Gcm] {
        let json = serde_json::to_string(variant).unwrap();
        let back: CipherType = serde_json::from_str(&json).unwrap();
        assert_eq!(*variant, back);
    }
}

#[test]
fn encrypted_data_serde_roundtrip() {
    use cryptonote::crypto::{CipherType, EncryptedData};
    let ed = EncryptedData {
        cipher: CipherType::Aes256Gcm,
        nonce: vec![1, 2, 3],
        ciphertext: vec![4, 5, 6],
        salt: vec![7, 8, 9],
    };
    let json = serde_json::to_string(&ed).unwrap();
    let back: EncryptedData = serde_json::from_str(&json).unwrap();
    assert_eq!(ed.cipher, back.cipher);
    assert_eq!(ed.nonce, back.nonce);
    assert_eq!(ed.ciphertext, back.ciphertext);
    assert_eq!(ed.salt, back.salt);
}

#[test]
fn file_entry_serde_roundtrip() {
    use cryptonote::archive::FileEntry;
    let fe = FileEntry {
        name: "file.txt".into(),
        size: 42,
    };
    let json = serde_json::to_string(&fe).unwrap();
    let back: FileEntry = serde_json::from_str(&json).unwrap();
    assert_eq!(fe.name, back.name);
    assert_eq!(fe.size, back.size);
}

#[test]
fn archive_metadata_serde_roundtrip() {
    use cryptonote::archive::{ArchiveMetadata, FileEntry};
    use cryptonote::crypto::CipherType;
    let meta = ArchiveMetadata {
        cipher: CipherType::ChaCha20Poly1305,
        nonce: vec![10; 12],
        salt: vec![20; 32],
        files: vec![
            FileEntry {
                name: "a.txt".into(),
                size: 5,
            },
            FileEntry {
                name: "b.bin".into(),
                size: 100,
            },
        ],
    };
    let json = serde_json::to_string(&meta).unwrap();
    let back: ArchiveMetadata = serde_json::from_str(&json).unwrap();
    assert_eq!(meta.cipher, back.cipher);
    assert_eq!(meta.nonce, back.nonce);
    assert_eq!(meta.salt, back.salt);
    assert_eq!(meta.files, back.files);
}

#[test]
fn attachment_default() {
    use cryptonote::archive::Attachment;
    let att = Attachment::default();
    assert_eq!(att.name, "");
    assert!(att.data.is_empty());
}

#[test]
fn extract_archive_ignores_extra_entries() {
    use cryptonote::archive::*;
    use cryptonote::crypto::CipherType;
    use std::io::Write;
    use zip::write::FileOptions;
    use zip::CompressionMethod;
    let note = "test note";
    let pkg = create_archive_package(note, &[], "pw", CipherType::ChaCha20Poly1305).expect("Package creation failed");
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
    let files = extract_archive_package(&augmented, "pw").expect("Extraction with extra entries failed");
    assert_eq!(files.len(), 1);
    assert_eq!(String::from_utf8(files[0].data.clone()).unwrap(), note);
}
