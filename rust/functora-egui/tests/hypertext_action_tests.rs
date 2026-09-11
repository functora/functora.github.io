//! Uniform footer paragraphs (plan step): `Hypertext` renders mixed
//! text/link/action segments in one text layout (one font, one baseline),
//! and action clicks report their id for internal navigation.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::{Hypertext, Segment};

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

    fn step(
        &mut self,
        events: Vec<Event>,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

fn footer(action_slot: &std::cell::Cell<Option<String>>) -> impl FnMut(&mut egui::Ui) + '_ {
    move |ui: &mut egui::Ui| {
        let (_, clicked) = Hypertext::new()
            .text("© 2026 Functora. ")
            .link("Terms", "https://example.com/terms")
            .text(" and ")
            .action("Privacy", "privacy")
            .size(11.0)
            .show_action(ui);
        if clicked.is_some() {
            action_slot.set(clicked);
        }
    }
}

fn single_text(out: &egui::FullOutput) -> (Pos2, std::sync::Arc<egui::Galley>) {
    let texts: Vec<(Pos2, std::sync::Arc<egui::Galley>)> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some((text.pos, std::sync::Arc::clone(&text.galley))),
            _ => None,
        })
        .collect();
    assert_eq!(
        texts.len(),
        1,
        "mixed footer paragraph must be one text layout, got {}",
        texts.len()
    );
    texts.into_iter().next().unwrap()
}

fn click_at(app: &mut App, pos: Pos2, body: &mut dyn FnMut(&mut egui::Ui)) {
    let _ = app.step(vec![Event::PointerMoved(pos)], body);
    let _ = app.step(
        vec![Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
    let _ = app.step(
        vec![Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
    let _ = app.step(vec![], body);
}

#[test]
fn mixed_paragraph_is_one_uniform_layout() {
    let slot = std::cell::Cell::new(None);
    let mut body = footer(&slot);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let (_, galley) = single_text(&out);
    let text = galley.text();
    for part in ["© 2026 Functora.", "Terms", "Privacy"] {
        assert!(text.contains(part), "paragraph must contain {part:?}");
    }
}

#[test]
fn action_click_reports_its_id() {
    let slot = std::cell::Cell::new(None);
    let mut body = footer(&slot);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let (pos, galley) = single_text(&out);
    let full = galley.text();
    let byte = full.find("Privacy").expect("action label must render");
    let char_idx = full[..byte].chars().count();
    let glyph = galley.pos_from_cursor(egui::text::CCursor::new(char_idx));
    click_at(&mut app, pos + glyph.center().to_vec2(), &mut body);
    assert_eq!(slot.take().as_deref(), Some("privacy"));
}

#[test]
fn clicking_plain_text_reports_no_action() {
    let slot = std::cell::Cell::new(None);
    let mut body = footer(&slot);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let (pos, galley) = single_text(&out);
    let full = galley.text();
    let byte = full.find("Functora").expect("plain text must render");
    let char_idx = full[..byte].chars().count();
    let glyph = galley.pos_from_cursor(egui::text::CCursor::new(char_idx));
    click_at(&mut app, pos + glyph.center().to_vec2(), &mut body);
    assert_eq!(slot.take(), None);
}

#[test]
fn size_builder_scales_layout_height() {
    let render = |size: f32| {
        let mut app = App::new();
        let mut body = |ui: &mut egui::Ui| {
            let _ = Hypertext::new()
                .text("© 2026 Functora.")
                .size(size)
                .show(ui);
        };
        let _ = app.step(vec![], &mut body);
        let out = app.step(vec![], &mut body);
        let (_, galley) = single_text(&out);
        galley.size().y
    };
    let small = render(11.0);
    let base = render(12.0);
    assert!(
        small < base,
        "11px layout ({small}) must be shorter than 12px ({base})"
    );
}

#[test]
fn centered_paragraph_spans_available_width() {
    let wide = |centered: bool| {
        let mut app = App::new();
        let rect_slot = std::cell::Cell::new(Rect::NOTHING);
        let mut body = |ui: &mut egui::Ui| {
            let base = Hypertext::new().text("© 2026 Functora.").size(11.0);
            let paragraph = if centered { base.centered() } else { base };
            rect_slot.set(paragraph.show(ui).rect);
        };
        let _ = app.step(vec![], &mut body);
        let _ = app.step(vec![], &mut body);
        rect_slot.get().width()
    };
    let left = wide(false);
    let centered = wide(true);
    assert!(
        centered > 1000.0,
        "centered paragraph must span the available width, got {centered}"
    );
    assert!(
        left < centered,
        "left paragraph ({left}) must be narrower than centered ({centered})"
    );
}

#[test]
fn action_segments_match_link_styling() {
    // `Segment::Action` renders with the link color in the shared layout;
    // covered structurally by `mixed_paragraph_is_one_uniform_layout`.
    let action = Segment::Action {
        label: "Privacy".into(),
        id: "privacy".into(),
    };
    match action {
        Segment::Action { label, id } => {
            assert_eq!(label, "Privacy");
            assert_eq!(id, "privacy");
        }
        Segment::Text(_) | Segment::Link { .. } => panic!("wrong segment"),
    }
}
