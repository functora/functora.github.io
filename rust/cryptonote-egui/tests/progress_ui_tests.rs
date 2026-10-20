#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Global progress UI: every long operation renders one blocking overlay (the
//! file-picker-style modal) with the operation name, a progress bar and the
//! percentage, without byte or entry-name details.

use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::progress::{Job, Stage};
use functora_egui::i18n::Language;

fn collect_shape(shape: &egui::Shape, rendered: &mut String) {
    match shape {
        egui::Shape::Text(text) => {
            rendered.push_str(text.galley.text());
            rendered.push('\n');
        }
        egui::Shape::Vec(shapes) => {
            for inner in shapes {
                collect_shape(inner, rendered);
            }
        }
        _ => {}
    }
}

fn run_app(app: &mut CryptonoteApp) -> String {
    use egui::{Pos2, RawInput, Rect, Vec2};
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        ..Default::default()
    };
    let mut frame = eframe::Frame::_new_kittest();
    let mut output = ctx.run_ui(raw, |ui| {
        eframe::App::ui(app, ui, &mut frame);
    });
    output.textures_delta.clear();
    let mut rendered = String::new();
    for clipped in &output.shapes {
        collect_shape(&clipped.shape, &mut rendered);
    }
    rendered
}

/// Opens the real overlay via the test hook, then swaps in a precise job so
/// the legend text is deterministic.
fn app_with_open_overlay(job: Job<Stage>) -> CryptonoteApp {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Eng);
    app.open_progress_overlay_for_tests(job.stage);
    app.temporary.progress = Some(job);
    app
}

fn decrypt_job(name: Option<&str>) -> Job<Stage> {
    Job {
        stage: Stage::Decrypt,
        done: 26,
        total: 100,
        name: name.map(ToString::to_string),
    }
}

#[test]
fn overlay_shows_operation_name_and_percent() {
    let mut app = app_with_open_overlay(decrypt_job(None));
    let rendered = run_app(&mut app);
    assert!(
        rendered.contains("Decrypting..."),
        "overlay must show the operation name, got:\n{rendered}"
    );
    assert!(
        rendered.contains("26%"),
        "overlay must show the percentage, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("26 B / 100 B"),
        "overlay must not show byte details, got:\n{rendered}"
    );
}

#[test]
fn overlay_hides_entry_name_details() {
    let mut app = app_with_open_overlay(decrypt_job(Some("photo.png")));
    let rendered = run_app(&mut app);
    assert!(
        !rendered.contains("photo.png"),
        "overlay must not show entry name details, got:\n{rendered}"
    );
    assert!(
        rendered.contains("26%"),
        "overlay must show the percentage, got:\n{rendered}"
    );
}

#[test]
fn overlay_has_cancel_button() {
    let mut app = app_with_open_overlay(decrypt_job(None));
    let rendered = run_app(&mut app);
    assert!(
        rendered.contains("Cancel"),
        "overlay must offer cancel, got:\n{rendered}"
    );
}

#[test]
fn no_progress_renders_no_legend() {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Eng);
    let rendered = run_app(&mut app);
    assert!(
        !rendered.contains("Decrypting..."),
        "idle app must not render a progress legend, got:\n{rendered}"
    );
}

#[test]
fn operation_name_follows_language() {
    let mut app = app_with_open_overlay(decrypt_job(None));
    app.set_language(Language::Rus);
    let rendered = run_app(&mut app);
    assert!(
        rendered.contains("Расшифровка..."),
        "operation name must follow the selected language, got:\n{rendered}"
    );
    assert!(
        rendered.contains("26%"),
        "percentage must render regardless of language, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("Decrypting..."),
        "stale English operation name must not leak, got:\n{rendered}"
    );
}
