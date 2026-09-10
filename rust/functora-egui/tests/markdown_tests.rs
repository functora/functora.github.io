#![cfg(feature = "markdown")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Markdown rendering: headings, emphasis, links, tables and code render to
//! native text; empty and untrusted input renders without panic.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::{CommonMarkCache, CommonMarkViewer};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

struct App {
    ctx: Context,
    frame: u32,
}

impl App {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
        });
        out.textures_delta.clear();
        out
    }
}

fn texts(output: &egui::FullOutput) -> Vec<String> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect()
}

fn rendered(app: &mut App, cache: &mut CommonMarkCache, source: &str) -> Vec<String> {
    let mut body = |ui: &mut egui::Ui| {
        let _ = CommonMarkViewer::new().show(ui, cache, source);
    };
    texts(&app.step(&mut body))
}

#[test]
fn headings_emphasis_and_links_render() {
    let mut app = App::new();
    let mut cache = CommonMarkCache::default();
    let found = rendered(
        &mut app,
        &mut cache,
        "# Shopping\n\nBuy **Milk** and *bread* from [example](https://example.com).",
    );
    for needle in ["Shopping", "Milk", "bread", "example"] {
        assert!(
            found.iter().any(|text| text.contains(needle)),
            "rendered markdown must contain {needle:?}, got {found:?}",
        );
    }
}

#[test]
fn tables_and_code_render() {
    let mut app = App::new();
    let mut cache = CommonMarkCache::default();
    let found = rendered(
        &mut app,
        &mut cache,
        "| Alpha | Beta |\n|---|---|\n| one | two |\n\nUse `code` here.",
    );
    for needle in ["Alpha", "Beta", "one", "two", "code"] {
        assert!(
            found.iter().any(|text| text.contains(needle)),
            "rendered markdown must contain {needle:?}, got {found:?}",
        );
    }
}

#[test]
fn empty_and_untrusted_input_renders_without_panic() {
    let mut app = App::new();
    let mut cache = CommonMarkCache::default();
    let _ = rendered(&mut app, &mut cache, "");
    let found = rendered(
        &mut app,
        &mut cache,
        "<script>alert(1)</script>\n\n[click](javascript:alert(1))",
    );
    assert!(
        found.iter().any(|text| text.contains("click")),
        "link labels must render as text, got {found:?}",
    );
}

#[test]
fn cached_render_is_stable_across_frames() {
    let mut app = App::new();
    let mut cache = CommonMarkCache::default();
    let source = "## Cached\n\nStable **output** across frames.";
    let first = rendered(&mut app, &mut cache, source);
    let second = rendered(&mut app, &mut cache, source);
    assert_eq!(first, second, "cached render must be deterministic");
    assert!(
        first.iter().any(|text| text.contains("Cached")),
        "rendered markdown must contain the heading, got {first:?}",
    );
}
