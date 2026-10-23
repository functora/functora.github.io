//! `eframe::App::ui` renders with the selected persistent state: mid-frame
//! reads must see the chosen language (not browser-detected defaults), and an
//! idle frame must leave persistent state untouched.

use egui::{Pos2, RawInput, Rect, Vec2};
use functora_egui::i18n::Language;
use functora_egui::progress::{Job, Stage};
use functora_egui::theme_extra::Theme;
use functora_egui_demo::{ComponentId, ShowcaseApp};

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

fn run_app(app: &mut ShowcaseApp) -> String {
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 900.0))),
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

fn overlay_app(language: Language) -> ShowcaseApp {
    let mut app = ShowcaseApp::default();
    app.persistent.language = language;
    app.demo.blocking_overlay_open = true;
    app.blocking_overlay_job = Some(Job {
        stage: Stage::Download,
        done: 45,
        total: 100,
        name: Some("archive.zip".to_owned()),
    });
    app.navigate_to(Some(ComponentId::BlockingOverlay));
    app
}

#[test]
fn overlay_renders_in_selected_language() {
    for (language, label) in [
        (Language::Eng, "Cancel"),
        (Language::Spa, "Cancelar"),
        (Language::Rus, "Отмена"),
    ] {
        let rendered = run_app(&mut overlay_app(language));
        assert!(
            rendered.contains(label),
            "overlay must render its cancel label for {language:?}, got:\n{rendered}"
        );
    }
}

#[test]
fn idle_frame_leaves_persistent_state_unchanged() {
    let mut app = ShowcaseApp::default();
    app.persistent.theme = Theme::Dark;
    app.persistent.language = Language::Spa;
    let before = app.persistent.clone();
    _ = run_app(&mut app);
    assert_eq!(
        app.persistent, before,
        "an idle frame must not mutate persistent state"
    );
}
