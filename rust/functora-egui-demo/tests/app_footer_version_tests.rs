use egui::{Context, Event, PointerButton, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui_demo::{DONATE_ACTION, LIBRARY_CRATES_URL, LIBRARY_SOURCE_URL, ShowcaseApp};

mod common;
use common::rendered_text;

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

    fn step(&mut self, events: Vec<Event>) -> (egui::FullOutput, Option<String>) {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let action = std::cell::Cell::new(None);
        let mut output = self.ctx.run_ui(raw, |ui| {
            action.set(ShowcaseApp::show_app_footer(ui, Language::Eng));
        });
        output.textures_delta.clear();
        (output, action.take())
    }
}

fn label_position(output: &egui::FullOutput, label: &str) -> Pos2 {
    let text = output
        .shapes
        .iter()
        .find_map(|clipped| match &clipped.shape {
            Shape::Text(text) if text.galley.text().contains(label) => Some(text),
            _ => None,
        })
        .unwrap_or_else(|| panic!("the app footer must render {label:?}"));
    let full = text.galley.text();
    let byte = full
        .find(label)
        .unwrap_or_else(|| panic!("the app footer must render {label:?}"));
    let char_idx = full[..byte].chars().count();
    let glyph = text
        .galley
        .pos_from_cursor(egui::text::CCursor::new(char_idx));
    text.pos + glyph.center().to_vec2()
}

fn click_at(app: &mut App, position: Pos2) -> Vec<(egui::FullOutput, Option<String>)> {
    let press = |pressed| Event::PointerButton {
        pos: position,
        button: PointerButton::Primary,
        pressed,
        modifiers: egui::Modifiers::default(),
    };
    [
        vec![Event::PointerMoved(position)],
        vec![press(true)],
        vec![press(false)],
        vec![],
    ]
    .into_iter()
    .map(|events| app.step(events))
    .collect()
}

fn opened_url(outputs: &[(egui::FullOutput, Option<String>)]) -> Option<String> {
    outputs
        .iter()
        .flat_map(|(output, _)| &output.platform_output.commands)
        .find_map(|command| match command {
            egui::OutputCommand::OpenUrl(url) => Some(url.url.clone()),
            _ => None,
        })
}

#[test]
fn app_footer_shows_real_app_version() {
    let text = rendered_text(|ui| {
        _ = ShowcaseApp::show_app_footer(ui, Language::Eng);
    });
    assert!(
        text.contains(ShowcaseApp::DEMO_ATTRS.vsn),
        "app footer must show the demo crate version: {text}"
    );
    assert!(
        text.contains(&BaseMsg::VersionLabel.render(Language::Eng)),
        "app footer must label the version cryptonote-style: {text}"
    );
    assert!(
        text.contains(env!("CARGO_PKG_VERSION")),
        "app footer must derive from CARGO_PKG_VERSION: {text}"
    );
}

#[test]
fn app_footer_has_the_same_library_links_as_the_home_page() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let text = rendered_text(|ui| {
            _ = ShowcaseApp::show_app_footer(ui, lang);
        });
        let source = BaseMsg::SourceCodeButton.render(lang);
        let donate = BaseMsg::Donate.render(lang);
        for label in [source.clone(), "crates.io".to_owned(), donate.clone()] {
            assert!(
                text.contains(&label),
                "app footer must offer {label:?} in {lang:?}: {text}"
            );
        }
        assert!(
            text.contains(&format!("{source}. crates.io. {donate}.")),
            "app footer must separate its links with dots in {lang:?}: {text}"
        );
    }
}

#[test]
fn app_footer_source_link_opens_the_library_repository() {
    let mut app = App::new();
    let (output, _) = app.step(vec![]);
    let position = label_position(&output, &BaseMsg::SourceCodeButton.render(Language::Eng));
    let outputs = click_at(&mut app, position);
    assert_eq!(opened_url(&outputs).as_deref(), Some(LIBRARY_SOURCE_URL));
}

#[test]
fn app_footer_crates_link_opens_the_library_crates_page() {
    let mut app = App::new();
    let (output, _) = app.step(vec![]);
    let position = label_position(&output, "crates.io");
    let outputs = click_at(&mut app, position);
    assert_eq!(opened_url(&outputs).as_deref(), Some(LIBRARY_CRATES_URL));
}

#[test]
fn app_footer_donate_link_reports_the_donate_action() {
    let mut app = App::new();
    let (output, _) = app.step(vec![]);
    let position = label_position(&output, &BaseMsg::Donate.render(Language::Eng));
    let outputs = click_at(&mut app, position);
    assert!(
        outputs
            .iter()
            .any(|(_, action)| action.as_deref() == Some(DONATE_ACTION))
    );
}
