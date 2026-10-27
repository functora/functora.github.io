//! Two `Textarea` widgets on one screen must not share widget ids.
//!
//! Without a per-instance id both fields derive the same `ScrollArea` id, so
//! egui paints its id-clash diagnostics anchored to the field (text that
//! escapes it) and both fields scroll together.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

fn collect(shape: &egui::Shape, texts: &mut Vec<String>) {
    match shape {
        Shape::Text(text) => texts.push(text.galley.text().to_owned()),
        Shape::Vec(shapes) => {
            for inner in shapes {
                collect(inner, texts);
            }
        }
        _ => {}
    }
}

fn rendered(body: &mut dyn FnMut(&mut egui::Ui)) -> Vec<String> {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(360.0, 640.0))),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
    });
    out.textures_delta.clear();
    let mut texts = Vec::new();
    for clipped in &out.shapes {
        collect(&clipped.shape, &mut texts);
    }
    texts
}

#[test]
fn two_textareas_do_not_share_widget_ids() {
    let mut first = String::new();
    let mut second = String::new();
    let texts = rendered(&mut |ui| {
        let _ = functora_egui::Textarea::new(&mut first)
            .placeholder("first")
            .show(ui);
        ui.add_space(8.0);
        let _ = functora_egui::Textarea::new(&mut second)
            .placeholder("second")
            .min_height(80.0)
            .show(ui);
    });

    let clashes: Vec<_> = texts
        .iter()
        .filter(|text| text.contains("use of"))
        .collect();
    assert!(clashes.is_empty(), "egui reported id clashes: {clashes:?}");
}

#[test]
fn three_textareas_stay_distinct() {
    let mut fields = [String::new(), String::new(), String::new()];
    let texts = rendered(&mut |ui| {
        for field in &mut fields {
            let _ = functora_egui::Textarea::new(field)
                .placeholder("field")
                .min_height(60.0)
                .show(ui);
            ui.add_space(8.0);
        }
    });

    let clashes: Vec<_> = texts
        .iter()
        .filter(|text| text.contains("use of"))
        .collect();
    assert!(clashes.is_empty(), "egui reported id clashes: {clashes:?}");
}
