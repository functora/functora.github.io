//! Home action tabs: the fill-width Create/Open/Scan tabs must share one row
//! whenever their labels fit the screen.

use cryptonote::app::CryptonoteApp;

pub mod common;
use common::{UiDriver, all_texts, rendered_labels};

const HEIGHT: f32 = 800.0;
const WIDTHS: [f32; 5] = [320.0, 360.0, 400.0, 800.0, 1280.0];

fn lang() -> functora_egui::i18n::Language {
    cryptonote::detect_browser_language()
}

fn labels() -> Vec<String> {
    rendered_labels(
        &[
            cryptonote::Msg::ActionCreate,
            cryptonote::Msg::ActionOpen,
            cryptonote::Msg::ActionScan,
        ],
        lang(),
    )
}

fn collect_rects(shape: &egui::Shape, out: &mut Vec<(egui::Rect, egui::Color32)>) {
    match shape {
        egui::Shape::Vec(shapes) => shapes.iter().for_each(|s| collect_rects(s, out)),
        egui::Shape::Rect(rect) => out.push((rect.rect, rect.fill)),
        _ => {}
    }
}

/// Painted height of each action tab: the smallest opaque box around its
/// label galley.
#[must_use]
fn tab_heights(out: &egui::FullOutput, labels: &[String]) -> Vec<f32> {
    let mut texts = Vec::new();
    let mut boxes = Vec::new();
    for clipped in &out.shapes {
        common::collect_texts(&clipped.shape, &mut texts);
        collect_rects(&clipped.shape, &mut boxes);
    }
    labels
        .iter()
        .map(|label| {
            let galley = texts
                .iter()
                .filter(|(text, _)| text == label)
                .map(|(_, rect)| *rect)
                .min_by(|a, b| a.area().total_cmp(&b.area()))
                .unwrap_or_else(|| panic!("{label:?} must render; got {texts:?}"));
            boxes
                .iter()
                .filter(|(rect, fill)| fill.a() != 0 && rect.width() < 900.0 && rect.height() < 400.0)
                .map(|(rect, _)| *rect)
                .filter(|rect| rect.contains_rect(galley))
                .min_by(|a, b| a.area().total_cmp(&b.area()))
                .unwrap_or_else(|| panic!("{label:?} has no button box around {galley:?}"))
                .height()
        })
        .collect()
}

/// Painted tab heights on the last frame of the plan.
#[must_use]
fn tab_heights_after(plan: &[(f32, usize)]) -> Vec<f32> {
    let mut app = CryptonoteApp::default();
    let mut driver = UiDriver::with_fonts();
    let labels = labels();
    let mut last = None::<egui::FullOutput>;
    for (width, frames) in plan {
        for _ in 0..*frames {
            last = Some(driver.run_sized(&mut app, egui::Vec2::new(*width, HEIGHT), vec![]));
        }
    }
    let out = last.unwrap_or_default();
    tab_heights(&out, &labels)
}

/// Panics unless the strip settles onto its current-width height.
///
/// # Panics
/// When resize history changes the settled tab heights.
pub fn assert_recovers(what: &str, via_resize: &[f32], natural: &[f32]) {
    for (resize, natural_height) in via_resize.iter().zip(natural) {
        assert!(
            (resize - natural_height).abs() < 0.5,
            "{what}: tab heights {via_resize:?} after resize history must equal {natural:?}"
        );
    }
}

#[test]
fn home_action_tabs_recover_after_widen() {
    let natural_480 = tab_heights_after(&[(480.0, 10)]);
    let via_resize_480 = tab_heights_after(&[(1280.0, 6), (320.0, 8), (480.0, 10)]);
    assert_recovers("widen from a wrapped narrow strip", &via_resize_480, &natural_480);

    let natural_640 = tab_heights_after(&[(640.0, 10)]);
    let via_resize_640 = tab_heights_after(&[(1280.0, 6), (320.0, 8), (640.0, 10)]);
    assert_recovers("widen to desktop-ish width", &via_resize_640, &natural_640);

    let natural_900 = tab_heights_after(&[(900.0, 10)]);
    let via_resize_900 = tab_heights_after(&[(1280.0, 6), (320.0, 8), (900.0, 10)]);
    assert_recovers("widen further past wrap threshold", &via_resize_900, &natural_900);
}

#[test]
fn home_action_tabs_recover_after_round_trip() {
    let natural_640 = tab_heights_after(&[(640.0, 10)]);
    let plan = [(1280.0, 6), (320.0, 8), (640.0, 8), (1280.0, 8), (640.0, 10)];
    let via_round_trip = tab_heights_after(&plan);
    assert_recovers(
        "round trip through a wrapped narrow strip",
        &via_round_trip,
        &natural_640,
    );
}

#[test]
fn home_action_tabs_share_one_row() {
    let labels = labels();
    for width in WIDTHS {
        let mut app = CryptonoteApp::default();
        let mut driver = UiDriver::with_fonts();
        let size = egui::Vec2::new(width, HEIGHT);
        let _ = driver.run_sized(&mut app, size, vec![]);
        let out = driver.run_sized(&mut app, size, vec![]);
        let texts = all_texts(&out);
        let rows: Vec<f32> = labels
            .iter()
            .map(|label| {
                texts
                    .iter()
                    .find_map(|(text, rect)| (text == label).then(|| rect.center().y))
                    .unwrap_or_else(|| panic!("{label:?} must render; got {texts:?}"))
            })
            .collect();
        assert!(
            rows.iter().all(|y| (y - rows[0]).abs() < 8.0),
            "action tabs that fit must share one row at {width}px, got {rows:?}"
        );
    }
}
