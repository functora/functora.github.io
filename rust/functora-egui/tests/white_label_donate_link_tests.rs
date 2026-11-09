//! White-label donate screen: the first mention of the author organization
//! links to the author website, exactly like the footer link.

#![cfg(all(feature = "qr", feature = "clipboard"))]

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::ToastState;
use functora_egui::i18n::Language;
use functora_egui::messages::Msg;
use functora_egui::white_label::{AppAttrs, WhiteLabelContent, screen_donate};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);
const AUTHOR_URL: &str = "https://functora.github.io/";

const ATTRS: AppAttrs = AppAttrs {
    app: "cryptonote-egui",
    vsn: "0.1.10",
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
};

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

fn donate_screen(content: WhiteLabelContent<Msg>) -> impl FnMut(&mut egui::Ui) {
    move |ui: &mut egui::Ui| {
        let mut toast = ToastState::new();
        screen_donate(ui, &ATTRS, Language::Eng, &content, 0.0, &mut toast);
    }
}

fn galley_with(out: &egui::FullOutput, needle: &str) -> (Pos2, std::sync::Arc<egui::Galley>) {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some((text.pos, std::sync::Arc::clone(&text.galley))),
            _ => None,
        })
        .find(|(_, galley)| galley.text().contains(needle))
        .unwrap_or_else(|| panic!("the donate screen must render {needle:?}"))
}

fn glyph_center(galley: &egui::Galley, needle: &str, from_end: bool) -> Vec2 {
    let text = galley.text();
    let byte = if from_end {
        text.rfind(needle)
    } else {
        text.find(needle)
    }
    .unwrap_or_else(|| panic!("the paragraph must contain {needle:?}"));
    let char_idx = text[..byte].chars().count();
    galley
        .pos_from_cursor(egui::text::CCursor::new(char_idx))
        .center()
        .to_vec2()
}

fn click_at(app: &mut App, pos: Pos2, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
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
    app.step(
        vec![Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    )
}

fn opened_urls(out: &egui::FullOutput) -> Vec<String> {
    out.platform_output
        .commands
        .iter()
        .filter_map(|command| match command {
            egui::OutputCommand::OpenUrl(open_url) => Some(open_url.url.clone()),
            egui::OutputCommand::CopyText(_)
            | egui::OutputCommand::TextSelectionSettled(_)
            | egui::OutputCommand::CopyImage(_) => None,
        })
        .collect()
}

#[test]
fn donate_intro_links_the_first_author_mention() {
    let content = WhiteLabelContent::<Msg> {
        donate_greeting: Some(Msg::DonateGreeting),
        donate_intro: Some(Msg::DonateIntro),
        ..Default::default()
    };
    let mut body = donate_screen(content);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let frame = app.step(vec![], &mut body);
    let (pos, galley) = galley_with(&frame, "Sincerely yours");
    let link = pos + glyph_center(&galley, "Functora", false);
    let clicked = click_at(&mut app, link, &mut body);
    assert_eq!(opened_urls(&clicked), vec![AUTHOR_URL.to_string()]);
}

#[test]
fn donate_intro_keeps_the_closing_mention_plain() {
    let content = WhiteLabelContent::<Msg> {
        donate_greeting: Some(Msg::DonateGreeting),
        donate_intro: Some(Msg::DonateIntro),
        ..Default::default()
    };
    let mut body = donate_screen(content);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let frame = app.step(vec![], &mut body);
    let (pos, galley) = galley_with(&frame, "Sincerely yours");
    let closing = pos + glyph_center(&galley, "Functora", true);
    let clicked = click_at(&mut app, closing, &mut body);
    assert!(
        opened_urls(&clicked).is_empty(),
        "only the first mention may link: {:?}",
        opened_urls(&clicked)
    );
}

#[test]
fn donate_intro_is_one_mixed_layout_with_theme_colors() {
    let theme = functora_egui::theme::shadcn_theme_light::light();
    let content = WhiteLabelContent::<Msg> {
        donate_greeting: Some(Msg::DonateGreeting),
        donate_intro: Some(Msg::DonateIntro),
        ..Default::default()
    };
    let mut body = donate_screen(content);
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let frame = app.step(vec![], &mut body);
    let (_, galley) = galley_with(&frame, "Sincerely yours");
    let sections = &galley.job.sections;
    assert_eq!(
        sections.len(),
        3,
        "the intro must be one text/link/text layout"
    );
    let link_range = &sections[1].byte_range;
    assert_eq!(
        &galley.job.text[link_range.start.0..link_range.end.0],
        "Functora"
    );
    assert_eq!(sections[1].format.color, theme.primary);
    assert_eq!(sections[0].format.color, theme.foreground);
    assert_eq!(sections[2].format.color, theme.foreground);
}

#[test]
fn default_donate_greeting_links_the_author_mention() {
    let mut body = donate_screen(WhiteLabelContent::<Msg>::default());
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let frame = app.step(vec![], &mut body);
    let (pos, galley) = galley_with(&frame, "Hello, User!");
    let link = pos + glyph_center(&galley, "functora", false);
    let clicked = click_at(&mut app, link, &mut body);
    assert_eq!(opened_urls(&clicked), vec![AUTHOR_URL.to_string()]);
}

#[test]
fn default_donate_intro_keeps_the_closing_mention_plain() {
    let mut body = donate_screen(WhiteLabelContent::<Msg>::default());
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let frame = app.step(vec![], &mut body);
    let (pos, galley) = galley_with(&frame, "If you're enjoying this software");
    let closing = pos + glyph_center(&galley, "functora", true);
    let clicked = click_at(&mut app, closing, &mut body);
    assert!(
        opened_urls(&clicked).is_empty(),
        "only the first mention may link: {:?}",
        opened_urls(&clicked)
    );
}
