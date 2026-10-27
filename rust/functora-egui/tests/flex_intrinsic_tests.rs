//! Flex items must report their intrinsic (non-wrapped) size, so that a
//! stale per-item slot recorded by the flex container - e.g. after
//! navigating between screens that reuse the same flex id - can never
//! lock a widget into a wrapped, degenerate shape.
#![cfg(not(target_arch = "wasm32"))]

pub mod common;
use common::{Harness, all_texts};

use egui::{Pos2, Rect, Vec2};
use functora_egui::{Avatar, Badge, Button, Checkbox, Flex, Kbd, Skeleton, Spinner};
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

const LINE_HEIGHT: f32 = 20.0;

fn label_heights(out: &egui::FullOutput, labels: &[&str]) -> Vec<(f32, String)> {
    all_texts(out)
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
    let mut app = Harness::new(Vec2::new(800.0, 600.0));
    for _ in 0..3 {
        let out = app.step(&mut |ui| {
            screen_buttons(ui);
        });
        for (height, text) in label_heights(&out, &labels) {
            assert!(height <= LINE_HEIGHT, "{text:?} must stay single-line");
        }
    }
    for _ in 0..3 {
        let _ = app.step(&mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for frames_after in 0..5 {
        let out = app.step(&mut |ui| {
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
    let mut app = Harness::new(Vec2::new(800.0, 600.0));
    let mut value = false;
    for _ in 0..3 {
        let _ = app.step(&mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for _ in 0..3 {
        let out = app.step(&mut |ui| {
            screen_mixed_widgets(&mut value, ui);
        });
        assert_eq!(label_heights(&out, &labels).len(), 2);
    }
    for _ in 0..3 {
        let _ = app.step(&mut |ui| {
            screen_narrow_buttons(ui);
        });
    }
    for frames_after in 0..5 {
        let out = app.step(&mut |ui| {
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
    let screen = Vec2::new(240.0, 400.0);
    let mut app = Harness::new(screen);
    let mut out = egui::FullOutput::default();
    for _ in 0..3 {
        out = app.step(&mut |ui| {
            let _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
                let _ = f.add(Button::new("An extremely long button title"));
            });
        });
    }
    for (rect, text) in all_texts(&out) {
        assert!(
            rect.max.x <= screen.x + 1.0,
            "squeezed button title {text:?} at {rect:?} escapes the container"
        );
    }
}

fn button_in_slot(text: &str, slot: f32) -> (Option<Vec2>, Vec2) {
    let ctx = egui::Context::default();
    let raw = egui::RawInput {
        screen_rect: Some(Rect::from_min_size(
            Pos2::ZERO,
            Vec2::new(slot.max(60.0), 400.0),
        )),
        ..Default::default()
    };
    let mut intrinsic = None;
    let mut painted = Vec2::ZERO;
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            let response = inner.add(functora_egui::Button::new(text.to_owned()));
            intrinsic = response.intrinsic_size();
            painted = response.rect.size();
        });
    });
    out.textures_delta.clear();
    (intrinsic, painted)
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00F1_E111),
        cases: 32,
        ..ProptestConfig::default()
    })]

    #[test]
    fn intrinsic_width_does_not_depend_on_the_slot(
        text in proptest::collection::vec(
            proptest::prop_oneof![any::<char>(), Just(' ')],
            0..60,
        ).prop_map(|chars: Vec<char>| chars.into_iter().collect::<String>()),
        slot_a in 60.0_f32..500.0,
        slot_b in 60.0_f32..500.0,
    ) {
        let (intrinsic_a, _) = button_in_slot(&text, slot_a);
        let (intrinsic_b, _) = button_in_slot(&text, slot_b);
        prop_assert!(
            (intrinsic_a.map_or(0.0, |s| s.x) - intrinsic_b.map_or(0.0, |s| s.x)).abs() < 0.5,
            "intrinsic width of {text:?} changed between {slot_a}px and {slot_b}px slots"
        );
    }

    #[test]
    fn intrinsic_size_covers_the_painted_rect(
        text in proptest::collection::vec(
            proptest::prop_oneof![any::<char>(), Just(' ')],
            0..60,
        ).prop_map(|chars: Vec<char>| chars.into_iter().collect::<String>()),
        slot in 60.0_f32..400.0,
    ) {
        let (reported, painted) = button_in_slot(&text, slot);
        let intrinsic = reported
            .unwrap_or_else(|| panic!("a button must report intrinsic size"));
        prop_assert!(
            intrinsic.x >= painted.x - 0.5 && intrinsic.y >= painted.y - 0.5,
            "intrinsic {intrinsic:?} does not cover the painted {painted:?} of {text:?}"
        );
    }
}
