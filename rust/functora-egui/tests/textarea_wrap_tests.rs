//! Textarea wrapping: rows break on word boundaries, so a line only splits
//! inside a word when that word does not fit the field on its own.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);
const TEXT: &str = "alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima";

type ArcGalley = std::sync::Arc<egui::Galley>;

fn galleys(out: &egui::FullOutput) -> Vec<ArcGalley> {
    fn collect(shape: &Shape, out: &mut Vec<ArcGalley>) {
        match shape {
            Shape::Text(text) => out.push(text.galley.clone()),
            Shape::Vec(shapes) => {
                for inner in shapes {
                    collect(inner, out);
                }
            }
            _ => {}
        }
    }
    let mut found = Vec::new();
    for clipped in &out.shapes {
        collect(&clipped.shape, &mut found);
    }
    found
}

fn row_texts(galley: &ArcGalley) -> Vec<String> {
    galley
        .rows
        .iter()
        .map(|row| row.glyphs.iter().map(|g| g.chr).collect())
        .collect()
}

fn split_word(source: &str, rows: &[String]) -> Option<String> {
    rows.windows(2).find_map(|pair| {
        let [left, right] = pair else { return None };
        let head: String = left
            .chars()
            .rev()
            .take_while(|c| !c.is_whitespace())
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        let tail: String = right.chars().take_while(|c| !c.is_whitespace()).collect();
        let cut = head.chars().last().is_some_and(char::is_alphanumeric)
            && tail.chars().next().is_some_and(char::is_alphanumeric)
            && source.contains(&format!("{head}{tail}"));
        cut.then(|| format!("{head}{tail}"))
    })
}

fn assert_words_stay_whole(out: &egui::FullOutput, width: f32) {
    let mut rows_seen = 0;
    let mut rendered = galleys(out);
    rendered.retain(|galley| galley.text() == TEXT);
    assert!(!rendered.is_empty(), "the textarea must render its text");
    for galley in &rendered {
        let rows = row_texts(galley);
        rows_seen += rows.len();
        if let Some(word) = split_word(TEXT, &rows) {
            panic!("word {word:?} is broken across rows at {width}px although it fits: {rows:?}");
        }
    }
    assert!(
        rows_seen > 1,
        "the sample must wrap onto several rows at {width}px, got {rows_seen}"
    );
}

#[test]
fn textarea_wraps_on_word_boundaries() {
    for width in [220.0_f32, 300.0, 380.0] {
        let mut text = TEXT.to_owned();
        let ctx = Context::default();
        functora_egui::setup_fonts(&ctx);
        let mut rendered = egui::FullOutput::default();
        for frame in 0..3 {
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
                time: Some(f64::from(frame) / 60.0),
                ..Default::default()
            };
            let mut out = ctx.run_ui(raw, |ui| {
                let _ = egui::CentralPanel::default().show(ui, |inner| {
                    let _ = functora_egui::Textarea::new(&mut text)
                        .desired_width(width)
                        .show(inner);
                });
            });
            out.textures_delta.clear();
            rendered = out;
        }
        assert_words_stay_whole(&rendered, width);
    }
}
