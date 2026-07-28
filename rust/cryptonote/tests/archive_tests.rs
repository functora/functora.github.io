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
