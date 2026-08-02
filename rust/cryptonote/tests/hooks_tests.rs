use cryptonote::archive::Attachment;
use cryptonote::components::*;
use cryptonote::{add_attachment, build_external, extract_archive_package, format_size, CipherType, NoteData};

#[test]
fn build_external_plaintext_builds_url_and_qr() {
    let external = build_external("hello world", "", None, &[]).unwrap();
    let External::Note(n) = external else {
        panic!("Expected a note artifact");
    };
    assert!(matches!(n.data, NoteData::PlainText(_)));
    assert!(n.url.contains("note="));
    assert!(n.url.contains("screen=open"));
    assert!(!n.qr.is_empty());
}

#[test]
fn build_external_encrypted_note_builds_url() {
    let external = build_external("secret", "pw", Some(CipherType::Aes256Gcm), &[]).unwrap();
    let External::Note(n) = external else {
        panic!("Expected a note artifact");
    };
    assert!(matches!(n.data, NoteData::CipherText(_)));
    assert!(n.url.contains("note="));
}

#[test]
fn build_external_archive_builds_pkg() {
    let external = build_external(
        "note",
        "pw",
        Some(CipherType::Aes256Gcm),
        &[Attachment {
            name: "a.bin".into(),
            data: vec![1, 2, 3],
        }],
    )
    .unwrap();
    assert!(matches!(external, External::Archive(_)));
}

#[test]
fn build_external_with_attachments_builds_archive_with_files() {
    let atts = vec![
        Attachment {
            name: "photo.jpg".into(),
            data: vec![1, 2, 3],
        },
        Attachment {
            name: "data.bin".into(),
            data: vec![9, 9, 9],
        },
    ];
    let external = build_external("note with files", "pw", Some(CipherType::ChaCha20Poly1305), &atts).unwrap();
    let External::Archive(a) = external else {
        panic!("Expected an archive artifact");
    };
    let (text, files) = extract_archive_package(&a.untag(), "pw").unwrap();
    assert_eq!(text, "note with files");
    assert_eq!(files, atts);
}

#[test]
fn build_external_oversized_note_falls_back_to_archive() {
    let note = "x".repeat(20_000);
    let external = build_external(&note, "", None, &[]).unwrap();
    let External::Archive(a) = external else {
        panic!("Expected an archive fallback");
    };
    let (text, files) = extract_archive_package(&a.untag(), "").unwrap();
    assert_eq!(text, note);
    assert!(files.is_empty());
}

#[test]
fn add_attachment_unique_name() {
    let mut atts = Vec::new();
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec(),
        },
    );
    assert_eq!(atts.len(), 2);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[1].name, "b.txt");
}

#[test]
fn add_attachment_duplicate_name_replaces() {
    let mut atts = vec![Attachment {
        name: "a.txt".into(),
        data: b"old".to_vec(),
    }];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"new".to_vec(),
        },
    );
    assert_eq!(atts.len(), 1);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data, b"new");
}

#[test]
fn add_attachment_duplicate_name_mixed_order() {
    let mut atts = vec![
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec(),
        },
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec(),
        },
    ];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one-replaced".to_vec(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two-replaced".to_vec(),
        },
    );
    assert_eq!(atts.len(), 2);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data, b"one-replaced");
    assert_eq!(atts[1].name, "b.txt");
    assert_eq!(atts[1].data, b"two-replaced");
}

#[test]
fn zero_bytes() {
    assert_eq!(format_size(0), "0 B");
}

#[test]
fn single_byte() {
    assert_eq!(format_size(1), "1 B");
}

#[test]
fn max_bytes() {
    assert_eq!(format_size(1023), "1023 B");
}

#[test]
fn exactly_one_kb() {
    assert_eq!(format_size(1024), "1.0 KB");
}

#[test]
fn fractional_kb() {
    assert_eq!(format_size(1536), "1.5 KB");
}

#[test]
fn nearly_one_mb() {
    assert_eq!(format_size(1048575), "1024.0 KB");
}

#[test]
fn exactly_one_mb() {
    assert_eq!(format_size(1048576), "1.0 MB");
}

#[test]
fn fractional_mb() {
    assert_eq!(format_size(2097152), "2.0 MB");
}

#[test]
fn large_mb_value() {
    assert_eq!(format_size(10_485_760), "10.0 MB");
}

#[test]
fn large_size() {
    let result = format_size(1_073_741_824);
    assert!(result.ends_with(" MB"));
}

#[test]
fn precision_half_mb() {
    assert_eq!(format_size(1572864), "1.5 MB");
}

#[test]
fn download_script_plain_filename() {
    let script = cryptonote::download_script("archive.cryptonote").unwrap();
    assert!(script.contains(r#"a.download="archive.cryptonote";"#));
    assert!(script.contains("dioxus.recv()"));
    assert!(script.contains("URL.createObjectURL"));
    assert!(!script.contains("base64"));
}

#[test]
fn download_script_has_no_payload_placeholder() {
    let script = cryptonote::download_script("x.cryptonote").unwrap();
    assert_eq!(script.matches("dioxus.recv()").count(), 1);
}

#[test]
fn download_script_escapes_quote_and_backslash() {
    let script = cryptonote::download_script("a\";alert(1);//").unwrap();
    assert!(!script.contains(r#"a.download="a";alert"#));
    assert!(script.contains(r#"a.download="a\";alert(1);//";"#));
}

#[test]
fn download_script_escapes_single_quote_and_html() {
    let script = cryptonote::download_script("b'</script><img onerror=alert(2)>").unwrap();
    assert!(!script.contains("</script>"));
    assert!(!script.contains("<img"));
    assert!(script.contains(r"\u003c"));
    assert!(script.contains(r"\u003e"));
    assert!(script.contains(r"\u0027"));
}

#[test]
fn download_script_escapes_newline() {
    let script = cryptonote::download_script("line1\nline2").unwrap();
    assert!(!script.contains("\n"));
    assert!(script.contains(r"line1\nline2"));
}
