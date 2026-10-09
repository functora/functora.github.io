#![allow(clippy::unwrap_used, clippy::expect_used)]
//! `QrScanner` is fully automatic: no Start/Stop/Clear buttons are rendered,
//! and the file-picker entry uses the translated catalog.

use egui::{Context, FullOutput, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::i18n::Language;

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

fn collect_shape(shape: &Shape, rendered: &mut Vec<String>) {
    match shape {
        Shape::Text(text) => rendered.push(text.galley.text().to_owned()),
        Shape::Vec(shapes) => {
            for inner in shapes {
                collect_shape(inner, rendered);
            }
        }
        _ => {}
    }
}

#[cfg(feature = "files")]
fn render(lang: Language) -> Vec<String> {
    let ctx = Context::default();
    let mut state = functora_egui::QrScannerState::new();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        ..Default::default()
    };
    let mut output: FullOutput = ctx.run_ui(raw, |ui| {
        _ = egui::CentralPanel::default().show(ui, |inner| {
            let _ = functora_egui::QrScanner::new()
                .auto_start(false)
                .show(inner, &mut state, lang);
        });
    });
    output.textures_delta.clear();
    let mut rendered = Vec::new();
    for clipped in &output.shapes {
        collect_shape(&clipped.shape, &mut rendered);
    }
    rendered
}

#[test]
#[cfg(feature = "files")]
fn scanner_renders_no_manual_controls() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let rendered = render(lang);
        for manual in ["Start", "Stop", "Clear"] {
            assert!(
                !rendered.iter().any(|text| text == manual),
                "manual control {manual:?} must not render in {lang:?}: {rendered:?}"
            );
        }
    }
}

#[test]
#[cfg(any(
    all(target_arch = "wasm32", feature = "web"),
    not(any(target_arch = "wasm32", target_os = "android"))
))]
#[cfg(feature = "files")]
fn scanner_offers_translated_pick_image() {
    use functora_egui::i18n::I18N;
    use functora_egui::messages::Msg;

    for (lang, label) in [
        (Language::Eng, "Pick Image"),
        (Language::Spa, "Elegir imagen"),
        (Language::Rus, "Выбрать изображение"),
    ] {
        assert_eq!(Msg::PickImage.render(lang), label);
        assert!(
            render(lang).iter().any(|text| text == label),
            "{label:?} must render in {lang:?}"
        );
    }
}
