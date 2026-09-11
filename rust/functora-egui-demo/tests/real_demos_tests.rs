//! Regression tests for real thumbnail/zip/crypto demos (plan step 3).
//!
//! These demos must call the real `functora-egui` APIs instead of returning
//! placeholder strings.

use functora_egui_demo::ShowcaseApp;

fn ctx() -> egui::Context {
    egui::Context::default()
}

#[test]
fn thumbnail_rejects_non_video_input() {
    match ShowcaseApp::make_thumbnail("data:image/png;base64,iVBORw0KGgo=") {
        Err(err) => assert!(
            err.contains("No thumbnail"),
            "unexpected error message: {err}"
        ),
        Ok(_) => panic!("non-video input must not produce a thumbnail"),
    }
}

#[test]
fn thumbnail_rejects_garbage_input() {
    assert!(
        ShowcaseApp::make_thumbnail("not a data url").is_err(),
        "garbage input must not produce a thumbnail"
    );
}

#[test]
fn thumbnail_poll_stores_image_on_ready() {
    let mut state = ShowcaseApp::default();
    let (tx, rx) = std::sync::mpsc::channel();
    state.platform.thumbnail_rx = Some(rx);
    assert!(
        tx.send(Ok(("bytes://thumb.jpg".to_owned(), b"fake-jpeg".to_vec())))
            .is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert!(state.platform.thumbnail_rx.is_none());
    let image = state.platform.thumbnail_image.as_ref();
    assert!(image.is_some(), "ready thumbnail must be stored");
    if let Some((uri, jpeg)) = image {
        assert_eq!(uri, "bytes://thumb.jpg");
        assert_eq!(*jpeg, b"fake-jpeg".to_vec());
    }
}

#[test]
fn thumbnail_poll_restores_pending_slot() {
    let mut state = ShowcaseApp::default();
    let (_tx, rx) = std::sync::mpsc::channel::<Result<(String, Vec<u8>), String>>();
    state.platform.thumbnail_rx = Some(rx);
    state.poll_platform_promises(&ctx());
    assert!(state.platform.thumbnail_rx.is_some());
}

#[test]
fn zip_roundtrip_verifies_matching_files() {
    let original = vec![
        ("a.txt".to_owned(), b"hello".to_vec()),
        ("b.bin".to_owned(), vec![0u8, 1u8, 2u8]),
    ];
    let unzipped = original.clone();
    match ShowcaseApp::verify_zip_roundtrip(&original, &unzipped) {
        Ok(text) => assert!(text.contains("2 files"), "unexpected summary: {text}"),
        Err(error) => panic!("matching files must verify: {error}"),
    }
}

#[test]
fn zip_roundtrip_rejects_tampered_content() {
    let original = vec![("a.txt".to_owned(), b"hello".to_vec())];
    let tampered = vec![("a.txt".to_owned(), b"evil".to_vec())];
    assert!(
        ShowcaseApp::verify_zip_roundtrip(&original, &tampered).is_err(),
        "tampered content must not verify"
    );
}

#[test]
fn zip_roundtrip_rejects_missing_file() {
    let original = vec![
        ("a.txt".to_owned(), b"hello".to_vec()),
        ("gone.txt".to_owned(), b"bye".to_vec()),
    ];
    let partial = vec![("a.txt".to_owned(), b"hello".to_vec())];
    assert!(
        ShowcaseApp::verify_zip_roundtrip(&original, &partial).is_err(),
        "missing file must not verify"
    );
}

#[test]
fn zip_poll_clears_ready_slot() {
    let mut state = ShowcaseApp::default();
    let (tx, rx) = std::sync::mpsc::channel();
    state.platform.zip_rx = Some(rx);
    assert!(
        tx.send(Ok("Zip ok: 1 file".to_owned())).is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert!(state.platform.zip_rx.is_none());
}

#[test]
fn crypto_encrypt_decrypt_roundtrip() {
    let note = match ShowcaseApp::encrypt_output("hello world", "s3cret") {
        Ok(note) => note,
        Err(error) => panic!("encryption must succeed: {error}"),
    };
    assert_ne!(note, "hello world");
    match ShowcaseApp::decrypt_output(&note, "s3cret") {
        Ok(back) => assert_eq!(back, "hello world"),
        Err(error) => panic!("decryption must succeed: {error}"),
    }
}

#[test]
fn crypto_decrypt_rejects_wrong_password() {
    let note = match ShowcaseApp::encrypt_output("hello world", "s3cret") {
        Ok(note) => note,
        Err(error) => panic!("encryption must succeed: {error}"),
    };
    assert!(
        ShowcaseApp::decrypt_output(&note, "wrong").is_err(),
        "wrong password must not decrypt"
    );
}

#[test]
fn crypto_decrypt_rejects_garbage_note() {
    assert!(
        ShowcaseApp::decrypt_output("definitely not a note", "s3cret").is_err(),
        "garbage note must not decrypt"
    );
}

#[test]
fn crypto_poll_sets_output_and_clears_op() {
    let mut state = ShowcaseApp::default();
    let (tx, rx) = std::sync::mpsc::channel();
    state.platform.crypto_rx = Some(rx);
    state.platform.crypto_op = Some(functora_egui_demo::CryptoOp::Encrypt);
    assert!(
        tx.send(Ok("note-bytes".to_owned())).is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert!(state.platform.crypto_rx.is_none());
    assert_eq!(state.platform.crypto_output, "note-bytes");
    assert!(state.platform.crypto_op.is_none());
}

#[test]
fn thumbnail_demo_shows_stored_image_info() {
    use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
    let mut state = ShowcaseApp::default();
    state.platform.thumbnail_image = Some(("bytes://thumb.jpg".to_owned(), b"fake-jpeg".to_vec()));
    let app_ctx = Context::default();
    let mut out = app_ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| state.demo_thumbnail(inner));
        },
    );
    out.textures_delta.clear();
    let labels: Vec<String> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect();
    assert!(
        labels.iter().any(|t| t.contains("9 bytes")),
        "stored thumbnail size must render, got: {labels:?}"
    );
}
