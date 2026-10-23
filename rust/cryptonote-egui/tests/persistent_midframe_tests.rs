//! `eframe::App::ui` must leave persistent state intact across an idle frame:
//! no take/restore churn and no spurious writes to the selected theme and
//! language.

use cryptonote_egui::app::CryptonoteApp;
use functora_egui::i18n::Language;
use functora_egui::theme_extra::Theme;

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

#[test]
fn idle_frame_leaves_persistent_state_unchanged() {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Spa);
    app.persistent.theme = Theme::Dark;
    let before = app.persistent.clone();
    let rendered = run_app(&mut app);
    assert_eq!(
        app.persistent, before,
        "an idle frame must not mutate persistent state, rendered:\n{rendered}"
    );
}
