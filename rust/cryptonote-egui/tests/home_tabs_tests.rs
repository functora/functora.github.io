//! Home action tabs: the fill-width Create/Open/Scan tabs must share one row
//! whenever their labels fit the screen.

use cryptonote_egui::app::CryptonoteApp;

pub mod common;
use common::{UiDriver, all_texts, rendered_labels};

const HEIGHT: f32 = 800.0;
const WIDTHS: [f32; 5] = [320.0, 360.0, 400.0, 800.0, 1280.0];

fn lang() -> functora_egui::i18n::Language {
    cryptonote_egui::detect_browser_language()
}

fn labels() -> Vec<String> {
    rendered_labels(
        &[
            cryptonote_egui::Msg::ActionCreate,
            cryptonote_egui::Msg::ActionOpen,
            cryptonote_egui::Msg::ActionScan,
        ],
        lang(),
    )
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
