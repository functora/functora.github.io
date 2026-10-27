//! Home dock: every action button stays a one-line-height unit and the dock
//! wraps whole buttons onto extra rows when they do not fit, instead of
//! wrapping the text inside a button.

use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::messages::Msg;
use functora_core::i18n::Language;
use functora_egui::i18n::I18N;

pub mod common;
use common::{UiDriver, all_texts};

const HEIGHT: f32 = 800.0;
const WIDTHS: [f32; 6] = [200.0, 240.0, 320.0, 360.0, 480.0, 1280.0];

fn dock_labels() -> Vec<String> {
    [
        Msg::Share.render(Language::Rus),
        Msg::AttachFiles.render(Language::Rus),
        Msg::ViewButton.render(Language::Rus),
    ]
    .to_vec()
}

fn render_dock(width: f32) -> Vec<(String, egui::Rect)> {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Rus);
    let mut driver = UiDriver::with_fonts();
    let size = egui::Vec2::new(width, HEIGHT);
    let _ = driver.run_sized(&mut app, size, vec![]);
    let _ = driver.run_sized(&mut app, size, vec![]);
    all_texts(&driver.run_sized(&mut app, size, vec![]))
        .into_iter()
        .filter(|(text, _)| dock_labels().contains(text))
        .collect()
}

#[test]
fn home_dock_buttons_stay_single_line() {
    for width in WIDTHS {
        for (text, rect) in render_dock(width) {
            assert!(
                rect.height() <= 20.0,
                "dock button {text:?} grew to {:.1}px at {width}px: \
                 dock buttons must keep one line height",
                rect.height()
            );
            assert!(
                rect.max.x <= width + 1.0,
                "dock button {text:?} at {rect:?} escapes a {width}px viewport"
            );
        }
    }
}

#[test]
fn home_dock_wraps_buttons_onto_rows_when_narrow() {
    let dock = render_dock(320.0);
    assert!(dock.len() >= 2, "dock buttons must render at 320px, got {dock:?}");
    let mut rows: Vec<f32> = Vec::new();
    for (_, rect) in &dock {
        if !rows.iter().any(|y| (y - rect.min.y).abs() < 8.0) {
            rows.push(rect.min.y);
        }
    }
    assert!(
        rows.len() >= 2,
        "the dock must wrap its buttons onto several rows at 320px, got {rows:?}"
    );
}

#[test]
fn home_dock_keeps_one_row_when_wide() {
    let dock = render_dock(1280.0);
    assert_eq!(dock.len(), 3, "every dock button must render at 1280px, got {dock:?}");
    let top = dock.first().map_or(0.0, |(_, rect)| rect.min.y);
    for (text, rect) in &dock {
        assert!(
            (rect.min.y - top).abs() < 8.0,
            "dock button {text:?} at {rect:?} must share one row at 1280px"
        );
    }
}
