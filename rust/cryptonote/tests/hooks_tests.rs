#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote::archive::{ArchiveSource, Attachment};
use cryptonote::components::*;
use cryptonote::{
    add_attachment, build_external, extract_archive_package_async, format_size, share_error, CipherType, NoteData,
};

mod common;

#[test]
fn share_error_requires_password_when_cipher_selected() {
    let error = share_error(Some(CipherType::Aes256Gcm), "");
    assert!(error.is_some(), "encrypted note without password must be blocked");
}

#[test]
fn share_error_allows_password_when_cipher_selected() {
    assert!(share_error(Some(CipherType::ChaCha20Poly1305), "pw").is_none());
}

#[test]
fn share_error_allows_plaintext_without_password() {
    assert!(share_error(None, "").is_none());
}

#[test]
fn build_external_plaintext_builds_url_and_qr() {
    common::with_runtime(|| {
        common::block_on(async {
            let external = build_external("hello world", "", None, &[], common::progress())
                .await
                .unwrap();
            let External::Note(n) = external else {
                panic!("Expected a note artifact");
            };
            assert!(matches!(n.data, NoteData::PlainText(_)));
            assert!(n.url.contains("note="));
            assert!(n.url.contains("screen=open"));
            assert!(!n.qr.is_empty());
        })
    });
}

#[test]
fn build_external_encrypted_note_builds_url() {
    common::with_runtime(|| {
        common::block_on(async {
            let external = build_external("secret", "pw", Some(CipherType::Aes256Gcm), &[], common::progress())
                .await
                .unwrap();
            let External::Note(n) = external else {
                panic!("Expected a note artifact");
            };
            assert!(matches!(n.data, NoteData::CipherText(_)));
            assert!(n.url.contains("note="));
        })
    });
}

#[test]
fn build_external_archive_builds_pkg() {
    common::with_runtime(|| {
        common::block_on(async {
            let external = build_external(
                "note",
                "pw",
                Some(CipherType::Aes256Gcm),
                &[Attachment {
                    name: "a.bin".into(),
                    data: vec![1, 2, 3].into(),
                }],
                common::progress(),
            )
            .await
            .unwrap();
            assert!(matches!(external, External::Archive(_)));
        })
    });
}

#[test]
fn build_external_with_attachments_builds_archive_with_files() {
    common::with_runtime(|| {
        common::block_on(async {
            let atts = vec![
                Attachment {
                    name: "photo.jpg".into(),
                    data: vec![1, 2, 3].into(),
                },
                Attachment {
                    name: "data.bin".into(),
                    data: vec![9, 9, 9].into(),
                },
            ];
            let external = build_external(
                "note with files",
                "pw",
                Some(CipherType::ChaCha20Poly1305),
                &atts,
                common::progress(),
            )
            .await
            .unwrap();
            let External::Archive(a) = external else {
                panic!("Expected an archive artifact");
            };
            let (text, files) =
                extract_archive_package_async(ArchiveSource::Bytes(a.untag()), "pw", common::progress())
                    .await
                    .unwrap();
            assert_eq!(text, "note with files");
            assert_eq!(files, atts);
        })
    });
}

#[test]
fn build_external_oversized_note_falls_back_to_archive() {
    common::with_runtime(|| {
        common::block_on(async {
            let note = "x".repeat(20_000);
            let external = build_external(&note, "", None, &[], common::progress()).await.unwrap();
            let External::Archive(a) = external else {
                panic!("Expected an archive fallback");
            };
            let (text, files) = extract_archive_package_async(ArchiveSource::Bytes(a.untag()), "", common::progress())
                .await
                .unwrap();
            assert_eq!(text, note);
            assert!(files.is_empty());
        })
    });
}

#[test]
fn add_attachment_unique_name() {
    let mut atts = Vec::new();
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec().into(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec().into(),
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
        data: b"old".to_vec().into(),
    }];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"new".to_vec().into(),
        },
    );
    assert_eq!(atts.len(), 1);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data.as_ref(), b"new");
}

#[test]
fn add_attachment_duplicate_name_mixed_order() {
    let mut atts = vec![
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec().into(),
        },
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec().into(),
        },
    ];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one-replaced".to_vec().into(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two-replaced".to_vec().into(),
        },
    );
    assert_eq!(atts.len(), 2);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data.as_ref(), b"one-replaced");
    assert_eq!(atts[1].name, "b.txt");
    assert_eq!(atts[1].data.as_ref(), b"two-replaced");
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
    assert_eq!(format_size(1_048_575), "1024.0 KB");
}

#[test]
fn exactly_one_mb() {
    assert_eq!(format_size(1_048_576), "1.0 MB");
}

#[test]
fn fractional_mb() {
    assert_eq!(format_size(2_097_152), "2.0 MB");
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
    assert_eq!(format_size(1_572_864), "1.5 MB");
}

#[test]
fn download_script_plain_filename() {
    let script = cryptonote::download_script("archive.cryptonote");
    assert!(script.contains(r#"a.download="archive.cryptonote";"#));
    assert!(script.contains("dioxus.recv()"));
    assert!(script.contains("URL.createObjectURL"));
    assert!(!script.contains("base64"));
}

#[test]
fn download_script_has_no_payload_placeholder() {
    let script = cryptonote::download_script("x.cryptonote");
    assert_eq!(script.matches("dioxus.recv()").count(), 1);
}

#[test]
fn download_script_escapes_quote_and_backslash() {
    let script = cryptonote::download_script("a\";alert(1);//");
    assert!(!script.contains(r#"a.download="a";alert"#));
    assert!(script.contains(r#"a.download="a\";alert(1);//";"#));
}

#[test]
fn download_script_escapes_single_quote_and_html() {
    let script = cryptonote::download_script("b'</script><img onerror=alert(2)>");
    assert!(!script.contains("</script>"));
    assert!(!script.contains("<img"));
    assert!(script.contains(r"\u003c"));
    assert!(script.contains(r"\u003e"));
    assert!(script.contains(r"\u0027"));
}

#[test]
fn download_script_escapes_newline() {
    let script = cryptonote::download_script("line1\nline2");
    assert!(!script.contains('\n'));
    assert!(script.contains(r"line1\nline2"));
}

#[test]
fn pick_script_chunked_protocol_with_size() {
    let script = functora_dioxus::files::pick_script(true);
    assert!(script.contains("t: 'begin'"));
    assert!(script.contains("size: f.size"));
    assert!(script.contains("t: 'chunk'"));
    assert!(script.contains("t: 'done'"));
    assert!(script.contains("f.slice("));
    assert!(script.contains("arrayBuffer()"));
    assert!(script.contains("2 * 1024 * 1024"));
    assert!(!script.contains("readAsDataURL"));
    assert!(!script.contains("FileReader"));
}

#[test]
fn pick_script_multiple_flag() {
    assert!(functora_dioxus::files::pick_script(true).contains("input.multiple = true"));
    assert!(functora_dioxus::files::pick_script(false).contains("input.multiple = false"));
}
