//! Toast text: a title or description made of one unbroken word must stay
//! inside the toast card, breaking between characters as a last resort,
//! instead of running past the card's rounded edge.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::{ToastState, ToastVariant};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);
const LONG_WORD: &str =
    "dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmcgdGhhdCBuZXZlciBmaXRzIG9uIGEgbGluZSBvZiB0ZXh0";

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

fn render_toast(title: String, description: Option<String>) -> Vec<(Rect, String)> {
    let ctx = Context::default();
    let mut toast = ToastState::new();
    match description {
        Some(desc) => toast.add_with_description(title, desc, ToastVariant::Error, 0.0),
        None => {
            toast.add(title, ToastVariant::Error, 0.0);
        }
    }
    let mut texts = Vec::new();
    for frame in 1..=2 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(frame) / 60.0),
            ..Default::default()
        };
        let mut out = ctx.run_ui(raw, |ui| toast.show(ui.ctx()));
        out.textures_delta.clear();
        texts.clear();
        for clipped in &out.shapes {
            collect_texts(&clipped.shape, &mut texts);
        }
    }
    texts
}

/// The card reserves 16px padding on both sides plus the close button for
/// the title row; text must fit inside the remaining width.
const CARD_WIDTH: f32 = 388.0;
const TITLE_ROOM: f32 = CARD_WIDTH - 32.0 - 14.0 - 8.0;
const DESCRIPTION_ROOM: f32 = CARD_WIDTH - 32.0;

#[test]
fn toast_title_with_one_long_word_stays_inside_the_card() {
    let rendered = render_toast(LONG_WORD.to_owned(), None);
    let rows: Vec<_> = rendered
        .iter()
        .filter(|(_, text)| text.contains(LONG_WORD))
        .collect();
    assert!(
        !rows.is_empty(),
        "the toast title must render, got {rendered:?}"
    );
    for (rect, text) in rows {
        assert!(
            rect.width() <= TITLE_ROOM + 2.0,
            "title {text:?} at {rect:?} is wider than the {TITLE_ROOM}px title room"
        );
    }
}

#[test]
fn toast_description_with_one_long_word_stays_inside_the_card() {
    let rendered = render_toast("Error".to_owned(), Some(LONG_WORD.to_owned()));
    let rows: Vec<_> = rendered
        .iter()
        .filter(|(_, text)| text.contains(LONG_WORD))
        .collect();
    assert!(
        !rows.is_empty(),
        "the toast description must render, got {rendered:?}"
    );
    for (rect, text) in rows {
        assert!(
            rect.width() <= DESCRIPTION_ROOM + 2.0,
            "description {text:?} at {rect:?} is wider than the {DESCRIPTION_ROOM}px card body"
        );
    }
}
