//! Flex items must report their intrinsic (non-wrapped) size, so that a
//! stale per-item slot recorded by the flex container - e.g. after
//! navigating between screens that reuse the same flex id - can never
//! lock a widget into a wrapped, degenerate shape.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::{Avatar, Badge, Button, Checkbox, Flex, Kbd, Skeleton, Spinner};

const LINE_HEIGHT: f32 = 20.0;

fn collect_texts(shape: &Shape, out: &mut Vec<(Rect, String)>) {
    match shape {
        Shape::Text(text) => out.push((
            text.galley.rect.translate(text.pos.to_vec2()),
            text.galley.text().to_owned(),
        )),
        Shape::Vec(shapes) => {
            for inner in shapes {
                collect_texts(inner, out);
            }
        }
        _ => {}
    }
}

fn texts(out: &egui::FullOutput) -> Vec<(Rect, String)> {
    let mut found = Vec::new();
    for clipped in &out.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
}

fn step(
    ctx: &Context,
    frame: &mut u32,
    screen: egui::Vec2,
    body: &mut dyn FnMut(&mut egui::Ui),
) -> egui::FullOutput {
    *frame += 1;
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
        time: Some(f64::from(*frame) / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
    });
    out.textures_delta.clear();
    out
}

fn label_heights(out: &egui::FullOutput, labels: &[&str]) -> Vec<(f32, String)> {
    texts(out)
        .into_iter()
        .filter(|(_, text)| labels.contains(&text.as_str()))
        .map(|(rect, text)| (rect.height(), text))
        .collect()
}

fn screen_buttons(ui: &mut egui::Ui) {
    let _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
        for label in [
            "Button",
            "ButtonGroup",
            "Checkbox",
            "Radio",
            "RadioGroup",
            "Flex",
        ] {
            let _ = f.add(Button::new(label));
        }
    });
}

fn screen_narrow_buttons(ui: &mut egui::Ui) {
    let _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
        for label in ["Some Long Variant", "XS", "Small"] {
            let _ = f.add(Button::new(label));
        }
    });
}

fn screen_mixed_widgets(value: &mut bool, ui: &mut egui::Ui) {
    let _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
        let _ = f.add(Avatar::new("A").size(48.0));
        let _ = f.add(Skeleton::new(60.0, 24.0));
        let _ = f.add(Spinner::new().size(20.0));
        let _ = f.add(Badge::new("Some Long Variant"));
        let _ = f.add(Kbd::new("Ctrl"));
        let _ = f.add(Checkbox::new(value).label("Some Long Variant"));
    });
}

#[test]
fn button_labels_stay_single_line_after_the_flex_slot_changes_owner() {
    let labels = [
        "Button",
        "ButtonGroup",
        "Checkbox",
        "Radio",
        "RadioGroup",
        "Flex",
    ];
    let ctx = Context::default();
    let mut frame = 0;
    for _ in 0..3 {
        let out = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_buttons(ui);
        });
        for (height, text) in label_heights(&out, &labels) {
            assert!(height <= LINE_HEIGHT, "{text:?} must stay single-line");
        }
    }
    for _ in 0..3 {
        let _ = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for frames_after in 0..5 {
        let out = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_buttons(ui);
        });
        let rendered = label_heights(&out, &labels);
        assert_eq!(rendered.len(), labels.len(), "every label must render");
        for (height, text) in rendered {
            assert!(
                height <= LINE_HEIGHT,
                "{text:?} wrapped to {height} {frames_after} frame(s) \
                 after the flex slot changed owner"
            );
        }
    }
}

#[test]
fn other_flex_widgets_keep_their_natural_shape_after_slot_reuse() {
    let labels = ["Some Long Variant"];
    let ctx = Context::default();
    let mut frame = 0;
    let mut value = false;
    for _ in 0..3 {
        let _ = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for _ in 0..3 {
        let out = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_mixed_widgets(&mut value, ui);
        });
        assert_eq!(label_heights(&out, &labels).len(), 2);
    }
    for _ in 0..3 {
        let _ = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for frames_after in 0..5 {
        let out = step(&ctx, &mut frame, egui::vec2(800.0, 600.0), &mut |ui| {
            screen_mixed_widgets(&mut value, ui);
        });
        let rendered = label_heights(&out, &labels);
        assert_eq!(rendered.len(), 2, "badge and checkbox labels must render");
        for (height, text) in rendered {
            assert!(
                height <= LINE_HEIGHT,
                "{text:?} wrapped to {height} {frames_after} frame(s) \
                 after the flex slot changed owner"
            );
        }
    }
}

#[test]
fn a_button_alone_in_a_narrow_row_stays_inside_the_container() {
    let ctx = Context::default();
    let mut frame = 0;
    let screen = Vec2::new(240.0, 400.0);
    let mut out = egui::FullOutput::default();
    for _ in 0..3 {
        out = step(&ctx, &mut frame, screen, &mut |ui| {
            let _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
                let _ = f.add(Button::new("An extremely long button title"));
            });
        });
    }
    for (rect, text) in texts(&out) {
        assert!(
            rect.max.x <= screen.x + 1.0,
            "squeezed button title {text:?} at {rect:?} escapes the container"
        );
    }
}
