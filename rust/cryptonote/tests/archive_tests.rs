use cryptonote::archive::{create_archive_package, extract_archive_package, Attachment};
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
    let pkg = create_archive_package(note, &attachments, "password", Some(CipherType::ChaCha20Poly1305))
        .expect("Package creation failed");

    let (note_text, files) = extract_archive_package(&pkg, "password").expect("Package extraction failed");
    assert_eq!(note_text, note);
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].name, "hello.txt");
    assert_eq!(files[0].data, b"Hello, World!");
    assert_eq!(files[1].name, "data.bin");
    assert_eq!(files[1].data, vec![1, 2, 3, 4, 5]);
}

#[test]
fn test_archive_roundtrip_aes() {
    let note = "AES note";
    let attachments = vec![Attachment {
        name: "file.txt".into(),
        data: b"content".to_vec(),
    }];
    let pkg = create_archive_package(note, &attachments, "strong_pw", Some(CipherType::Aes256Gcm))
        .expect("Package creation failed");
    let (note_text, files) = extract_archive_package(&pkg, "strong_pw").expect("Package extraction failed");
    assert_eq!(note_text, "AES note");
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].name, "file.txt");
    assert_eq!(files[0].data, b"content");
}

#[test]
fn test_archive_no_attachments() {
    let pkg = create_archive_package("Just a note", &[], "pw", Some(CipherType::ChaCha20Poly1305))
        .expect("Package creation failed");
    let (note_text, files) = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(note_text, "Just a note");
    assert!(files.is_empty());
}

#[test]
fn test_archive_wrong_password() {
    let pkg = create_archive_package("secret", &[], "correct_pw", Some(CipherType::Aes256Gcm))
        .expect("Package creation failed");
    let result = extract_archive_package(&pkg, "wrong_pw");
    assert!(result.is_err());
}

#[test]
fn test_archive_many_attachments() {
    let note = "Note with many files";
    let attachments: Vec<_> = (0..100)
        .map(|i| Attachment {
            name: format!("file_{}.bin", i),
            data: vec![i as u8; 100],
        })
        .collect();
    let pkg = create_archive_package(note, &attachments, "pw", Some(CipherType::ChaCha20Poly1305))
        .expect("Package creation failed");
    let (note_text, files) = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(note_text, note);
    assert_eq!(files.len(), 100);
    for (i, f) in files.iter().enumerate() {
        assert_eq!(f.name, format!("file_{}.bin", i));
        assert_eq!(f.data.len(), 100);
    }
}

#[test]
fn archive_metadata_serde_roundtrip() {
    use cryptonote::archive::ArchiveMetadata;
    use cryptonote::crypto::CipherType;
    let meta = ArchiveMetadata {
        cipher: Some(CipherType::ChaCha20Poly1305),
        nonce: vec![10; 12],
        salt: vec![20; 32],
    };
    let json = serde_json::to_string(&meta).unwrap();
    let back: ArchiveMetadata = serde_json::from_str(&json).unwrap();
    assert_eq!(meta.cipher, back.cipher);
    assert_eq!(meta.nonce, back.nonce);
    assert_eq!(meta.salt, back.salt);
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
    let pkg =
        create_archive_package(note, &[], "pw", Some(CipherType::ChaCha20Poly1305)).expect("Package creation failed");
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
    let (note_text, files) = extract_archive_package(&augmented, "pw").expect("Extraction with extra entries failed");
    assert_eq!(note_text, note);
    assert!(files.is_empty());
}

#[test]
fn test_archive_empty_note() {
    let pkg = create_archive_package("", &[], "pw", Some(CipherType::Aes256Gcm)).expect("Package creation failed");
    let (note_text, files) = extract_archive_package(&pkg, "pw").expect("Package extraction failed");
    assert_eq!(note_text, "");
    assert!(files.is_empty());
}

#[test]
fn test_plaintext_archive_roundtrip() {
    let note = "Plain note with attachments";
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
    let pkg = create_archive_package(note, &attachments, "", None).expect("Package creation failed");
    let (note_text, files) = extract_archive_package(&pkg, "").expect("Package extraction failed");
    assert_eq!(note_text, note);
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].name, "photo.png");
    assert_eq!(files[0].data, vec![9, 9, 9]);
    assert_eq!(files[1].name, "doc.txt");
    assert_eq!(files[1].data, b"plain");
}

#[test]
fn test_plaintext_archive_metadata_is_none() {
    let pkg = create_archive_package("plain", &[], "", None).expect("Package creation failed");
    let meta = cryptonote::archive::read_archive_metadata(&pkg).expect("Metadata read failed");
    assert_eq!(meta.cipher, None);
    assert!(meta.nonce.is_empty());
    assert!(meta.salt.is_empty());
}

#[test]
fn archive_metadata_old_format_parses_as_some() {
    let json = r#"{"cipher":"Aes256Gcm","nonce":[1,2,3],"salt":[4,5,6]}"#;
    let meta: cryptonote::archive::ArchiveMetadata = serde_json::from_str(json).expect("Old format parse failed");
    assert_eq!(meta.cipher, Some(CipherType::Aes256Gcm));
    assert_eq!(meta.nonce, vec![1, 2, 3]);
}
