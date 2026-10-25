pub mod common;
use common::collect_shape;
#[cfg(not(target_arch = "wasm32"))]
use common::run_async;

#[cfg(not(target_arch = "wasm32"))]
use cryptonote_egui::ArchiveSource;
use cryptonote_egui::Attachment;
#[cfg(not(target_arch = "wasm32"))]
use cryptonote_egui::crypto::CipherType;
#[cfg(not(target_arch = "wasm32"))]
use cryptonote_egui::{create_archive_package_async, extract_archive_package_async, load_archive_async};
use std::io::Write;
#[cfg(not(target_arch = "wasm32"))]
use std::sync::Arc;

#[cfg(not(target_arch = "wasm32"))]
fn attachments() -> Vec<Attachment> {
    vec![
        Attachment {
            name: "a.txt".to_owned(),
            data: Arc::from(b"alpha" as &[u8]),
        },
        Attachment {
            name: "nested/b.bin".to_owned(),
            data: Arc::from([1u8, 2u8, 3u8].as_slice()),
        },
    ]
}

#[cfg(not(target_arch = "wasm32"))]
fn assert_files(files: &[Attachment]) {
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].name, "a.txt");
    assert_eq!(files[0].data.to_vec(), b"alpha".to_vec());
    assert_eq!(files[1].name, "nested/b.bin");
    assert_eq!(files[1].data.to_vec(), vec![1u8, 2u8, 3u8]);
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn decrypt_encrypted_archive_without_note_preserves_attachments() {
    use cryptonote_egui::app::CryptonoteApp;
    use cryptonote_egui::route::Screen;

    fn ctx() -> egui::Context {
        egui::Context::default()
    }

    let package = run_async(async move {
        let files = attachments();
        create_archive_package_async(
            "",
            &files,
            "pw",
            Some(CipherType::Aes256Gcm),
            Arc::new(std::sync::atomic::AtomicBool::new(false)),
            |_| {},
        )
        .await
    })
    .unwrap_or_else(|err| panic!("package created: {err}"));
    let opened = run_async(load_archive_async(
        ArchiveSource::Bytes(package),
        Arc::new(std::sync::atomic::AtomicBool::new(false)),
        |_| {},
    ))
    .unwrap_or_else(|err| panic!("archive opened: {err}"));
    assert_eq!(opened.screen, Screen::Open);
    let mut app = CryptonoteApp::default();
    app.temporary.external = opened.external;
    app.temporary.password = "pw".to_owned();
    app.decrypt_current(0.0);
    let toasts_before = app.toast.next_id();
    for _ in 0..1000 {
        app.poll_receivers(&ctx());
        if app.temporary.screen == Screen::View || app.toast.next_id() != toasts_before {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    assert_eq!(app.temporary.note, "");
    assert_files(&app.temporary.attachments);
    assert_eq!(app.temporary.screen, Screen::View);
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn encrypted_archive_without_note_roundtrip() {
    let package = run_async(async move {
        let attached = attachments();
        create_archive_package_async(
            "",
            &attached,
            "pw",
            Some(CipherType::Aes256Gcm),
            Arc::new(std::sync::atomic::AtomicBool::new(false)),
            |_| {},
        )
        .await
    })
    .unwrap_or_else(|err| panic!("package created: {err}"));
    let (text, files) = run_async(extract_archive_package_async(
        ArchiveSource::Bytes(package),
        "pw",
        Arc::new(std::sync::atomic::AtomicBool::new(false)),
        |_| {},
    ))
    .unwrap_or_else(|err| panic!("archive extracted: {err}"));
    assert_eq!(text, "");
    assert_files(&files);
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn plain_archive_without_note_opens_view_with_files() {
    use cryptonote_egui::route::Screen;

    let package = run_async(async move {
        let attached = attachments();
        create_archive_package_async(
            "",
            &attached,
            "",
            None,
            Arc::new(std::sync::atomic::AtomicBool::new(false)),
            |_| {},
        )
        .await
    })
    .unwrap_or_else(|err| panic!("package created: {err}"));
    let opened = run_async(load_archive_async(
        ArchiveSource::Bytes(package),
        Arc::new(std::sync::atomic::AtomicBool::new(false)),
        |_| {},
    ))
    .unwrap_or_else(|err| panic!("archive opened: {err}"));
    assert_eq!(opened.screen, Screen::View);
    assert_eq!(opened.note, "");
    assert_files(&opened.attachments);
}

fn count_rects(shape: &egui::Shape) -> usize {
    match shape {
        egui::Shape::Rect(_) => 1,
        egui::Shape::Vec(shapes) => shapes.iter().map(count_rects).sum(),
        _ => 0,
    }
}

fn render_rects(app: &mut cryptonote_egui::app::CryptonoteApp) -> usize {
    use egui::{Pos2, RawInput, Rect, Vec2};
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        ..Default::default()
    };
    let mut output = ctx.run_ui(raw, |ui| {
        _ = egui::CentralPanel::default().show(ui, |inner| {
            app.screen_view(inner);
        });
    });
    output.textures_delta.clear();
    output.shapes.iter().map(|clipped| count_rects(&clipped.shape)).sum()
}

#[test]
fn view_hides_note_card_when_note_empty() {
    use cryptonote_egui::app::CryptonoteApp;
    use std::sync::Arc;

    let mut with_note = CryptonoteApp::default();
    with_note.temporary.note = "hello".to_owned();
    with_note.temporary.attachments = vec![Attachment {
        name: "a.txt".to_owned(),
        data: Arc::from(b"alpha" as &[u8]),
    }];
    let mut without_note = CryptonoteApp::default();
    without_note.temporary.attachments = vec![Attachment {
        name: "a.txt".to_owned(),
        data: Arc::from(b"alpha" as &[u8]),
    }];
    let full = render_rects(&mut with_note);
    let empty = render_rects(&mut without_note);
    assert!(
        empty < full,
        "empty note must not render the note card box (rects {empty} vs {full})"
    );
}

fn render_text(app: &mut cryptonote_egui::app::CryptonoteApp) -> String {
    use egui::{Pos2, RawInput, Rect, Vec2};
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        ..Default::default()
    };
    let mut output = ctx.run_ui(raw, |ui| {
        _ = egui::CentralPanel::default().show(ui, |inner| {
            app.screen_view(inner);
        });
    });
    output.textures_delta.clear();
    let mut rendered = String::new();
    for clipped in &output.shapes {
        collect_shape(&clipped.shape, &mut rendered);
    }
    rendered
}

#[test]
fn view_renders_attachments_without_note() {
    use cryptonote_egui::app::CryptonoteApp;

    let mut app = CryptonoteApp::default();
    app.temporary.note = String::new();
    app.temporary.attachments = vec![Attachment {
        name: "a.txt".to_owned(),
        data: std::sync::Arc::from(b"alpha" as &[u8]),
    }];
    let rendered = render_text(&mut app);
    assert!(rendered.contains("a.txt"), "attachment name must render without a note");
}

#[test]
fn view_renders_empty_state_without_panic() {
    use cryptonote_egui::app::CryptonoteApp;

    let mut app = CryptonoteApp::default();
    app.temporary.note = String::new();
    app.temporary.attachments = Vec::new();
    let _ = render_text(&mut app);
}

#[test]
fn missing_note_entry_defaults_to_empty_text() {
    let inner = {
        let mut writer = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
        writer
            .start_file("attachments/a.txt", zip::write::SimpleFileOptions::default())
            .unwrap_or_else(|err| panic!("inner entry: {err}"));
        writer
            .write_all(b"alpha")
            .unwrap_or_else(|err| panic!("inner bytes: {err}"));
        writer
            .finish()
            .unwrap_or_else(|err| panic!("inner zip: {err}"))
            .into_inner()
    };
    let package = {
        let mut writer = zip::ZipWriter::new(std::io::Cursor::new(Vec::new()));
        writer
            .start_file("metadata.json", zip::write::SimpleFileOptions::default())
            .unwrap_or_else(|err| panic!("metadata entry: {err}"));
        writer
            .write_all(br#"{"cipher":null,"kdf":"Argon2id","nonce":[],"salt":[]}"#)
            .unwrap_or_else(|err| panic!("metadata bytes: {err}"));
        writer
            .start_file("payload.cpt", zip::write::SimpleFileOptions::default())
            .unwrap_or_else(|err| panic!("payload entry: {err}"));
        writer
            .write_all(&inner)
            .unwrap_or_else(|err| panic!("payload bytes: {err}"));
        writer
            .finish()
            .unwrap_or_else(|err| panic!("outer zip: {err}"))
            .into_inner()
    };
    #[cfg(not(target_arch = "wasm32"))]
    {
        let (text, files) = run_async(extract_archive_package_async(
            ArchiveSource::Bytes(package),
            "",
            std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
            |_| {},
        ))
        .unwrap_or_else(|err| panic!("archive extracted: {err}"));
        assert_eq!(text, "");
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name, "a.txt");
        assert_eq!(files[0].data.to_vec(), b"alpha".to_vec());
    }
    #[cfg(target_arch = "wasm32")]
    {
        assert!(!package.is_empty());
    }
}
