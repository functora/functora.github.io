//! `text` module contract: wrapping modes, mode resolution and box sizing.
//!
//! Text must stay inside the width it is given, so that widgets can grow
//! taller instead of painting past their box.
#![cfg(not(target_arch = "wasm32"))]

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::text::Wrap;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;
use std::sync::Arc;

fn with_ui<R>(body: impl FnOnce(&mut egui::Ui) -> R) -> R {
    let ctx = Context::default();
    let mut slot = Some(body);
    let mut result = None;
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(800.0, 600.0))),
            ..Default::default()
        },
        |ui| {
            result = slot.take().map(|run| run(ui));
        },
    );
    out.textures_delta.clear();
    result.unwrap_or_else(|| panic!("ui body must run"))
}

fn galley(text: &str, max_width: f32, wrap: Option<Wrap>) -> Arc<egui::Galley> {
    with_ui(|ui| {
        functora_egui::text::layout(
            ui,
            text.to_owned(),
            egui::FontId::proportional(14.0),
            egui::Color32::WHITE,
            max_width,
            wrap,
        )
    })
}

const LONG: &str = "A rather long label that has to be wrapped somewhere on narrow mobile screens";
const LONG_WORD: &str =
    "dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmcgdGhhdCBuZXZlciBmaXRzIG9uIGEgbGluZSBvZiB0ZXh0";

#[test]
fn word_wrap_keeps_every_row_inside_the_width() {
    let galley = galley(LONG, 200.0, Some(Wrap::Word));
    assert!(
        galley.size().x <= 200.0,
        "wrapped text is {:?} wide",
        galley.size()
    );
    assert!(galley.rows.len() > 1, "long text must wrap onto more rows");
}

#[test]
fn word_wrap_breaks_words_that_cannot_fit() {
    let galley = galley(LONG_WORD, 120.0, Some(Wrap::Word));
    assert!(
        galley.size().x <= 120.0,
        "break-word fallback must not overflow: {:?}",
        galley.size()
    );
    assert!(galley.rows.len() > 1);
    assert!(!galley.elided, "word mode never drops text");
}

#[test]
fn any_mode_breaks_between_characters() {
    let galley = galley(LONG_WORD, 120.0, Some(Wrap::Any));
    assert!(galley.size().x <= 120.0, "got {:?}", galley.size());
    assert!(galley.rows.len() > 1);
}

#[test]
fn nowrap_stays_on_one_line_past_the_width() {
    let galley = galley(LONG, 120.0, Some(Wrap::Nowrap));
    assert_eq!(galley.rows.len(), 1, "nowrap must not wrap");
    assert!(
        galley.size().x > 120.0,
        "nowrap is expected to extend, got {:?}",
        galley.size()
    );
}

#[test]
fn truncate_stays_on_one_line_inside_the_width() {
    let galley = galley(LONG, 120.0, Some(Wrap::Truncate));
    assert_eq!(galley.rows.len(), 1, "truncate must not wrap");
    assert!(
        galley.size().x <= 120.0,
        "truncated text is {:?} wide",
        galley.size()
    );
    assert!(galley.elided, "truncated text must be elided");
}

#[test]
fn widget_override_beats_the_style_wrap_mode() {
    with_ui(|ui| {
        ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Extend);
        assert_eq!(
            functora_egui::text::resolve(ui, Some(Wrap::Truncate)),
            Wrap::Truncate
        );
    });
}

#[test]
fn style_wrap_mode_is_honored_without_an_override() {
    for (mode, expected) in [
        (egui::TextWrapMode::Extend, Wrap::Nowrap),
        (egui::TextWrapMode::Wrap, Wrap::Word),
        (egui::TextWrapMode::Truncate, Wrap::Truncate),
    ] {
        with_ui(|ui| {
            ui.style_mut().wrap_mode = Some(mode);
            assert_eq!(functora_egui::text::resolve(ui, None), expected);
        });
    }
}

#[test]
fn resolution_defaults_to_word_wrapping() {
    with_ui(|ui| {
        assert_eq!(functora_egui::text::resolve(ui, None), Wrap::Word);
    });
}

#[test]
fn box_height_keeps_single_line_padding_and_grows_with_rows() {
    let single = galley("Short", 400.0, None);
    let wrapped = galley(LONG, 200.0, None);
    let single_height = functora_egui::text::box_height(&single, 32.0);
    let wrapped_height = functora_egui::text::box_height(&wrapped, 32.0);
    assert!(
        (single_height - 32.0).abs() < 0.001,
        "a single line keeps the box height, got {single_height}"
    );
    assert!(
        wrapped_height > 32.0 && wrapped_height >= wrapped.size().y,
        "a wrapped box grows to {wrapped_height} for galley {:?}",
        wrapped.size()
    );
}

#[test]
fn natural_width_is_the_unbroken_width() {
    with_ui(|ui| {
        let font = egui::FontId::proportional(14.0);
        let natural = functora_egui::text::natural_width(ui.ctx(), LONG.to_owned(), font.clone());
        let nowrap = functora_egui::text::layout(
            ui,
            LONG.to_owned(),
            font,
            egui::Color32::WHITE,
            200.0,
            Some(Wrap::Nowrap),
        );
        assert!((natural - nowrap.size().x).abs() < f32::EPSILON);
        assert!(natural > 200.0, "the sample text is wider than 200px");
    });
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        cases: 64,
        ..ProptestConfig::default()
    })]

    #[test]
    fn word_wrap_never_exceeds_the_width(
        text in proptest::collection::vec(
            proptest::prop_oneof![any::<char>(), Just(' ')],
            0..160,
        ).prop_map(|chars: Vec<char>| chars.into_iter().collect::<String>()),
        max_width in 24.0_f32..400.0,
    ) {
        let galley = galley(&text, max_width, Some(Wrap::Word));
        // Rows round up to whole pixels, so a galley may come out a fraction
        // of a pixel wider than the width it was given, never more.
        prop_assert!(
            galley.size().x <= max_width + 1.0,
            "text {text:?} at width {max_width} rendered {:?}",
            galley.size()
        );
    }

    #[test]
    fn truncate_never_exceeds_the_width(
        text in proptest::collection::vec(any::<char>(), 0..80)
            .prop_map(|chars: Vec<char>| chars.into_iter().collect::<String>()),
        max_width in 24.0_f32..400.0,
    ) {
        let galley = galley(&text, max_width, Some(Wrap::Truncate));
        prop_assert!(
            galley.rows.len() == 1 && galley.size().x <= max_width + 1.0,
            "truncated text {text:?} at width {max_width} rendered {:?} in {} rows",
            galley.size(),
            galley.rows.len()
        );
    }
}
