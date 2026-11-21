//! The home page must offer three primary actions for the library sources,
//! the crates page and the donate screen with natural, localized text.

use egui::{Context, Event, PointerButton, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui_demo::route::AppRoute;
use functora_egui_demo::{
    CATEGORIES, CategoryId, LIBRARY_CRATES_URL, LIBRARY_SOURCE_URL, ShowcaseApp,
};

mod common;
use common::{rendered_text, rendered_text_sized};

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

    fn step(&mut self, app: &mut ShowcaseApp, events: Vec<Event>) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut output = self.ctx.run_ui(raw, |ui| {
            _ = app.render_component(ui, Language::Eng);
        });
        output.textures_delta.clear();
        output
    }
}

fn collect_texts(shape: &Shape, texts: &mut Vec<(String, Rect)>) {
    match shape {
        Shape::Text(text) => texts.push((
            text.galley.text().to_owned(),
            text.galley.rect.translate(text.pos.to_vec2()),
        )),
        Shape::Vec(shapes) => shapes.iter().for_each(|inner| collect_texts(inner, texts)),
        _ => {}
    }
}

fn text_position(output: &egui::FullOutput, label: &str) -> Pos2 {
    let mut texts = Vec::new();
    output
        .shapes
        .iter()
        .for_each(|clipped| collect_texts(&clipped.shape, &mut texts));
    texts
        .into_iter()
        .find(|(text, _)| text == label)
        .map_or_else(
            || panic!("the home page must render {label:?}"),
            |(_, rect)| rect.center(),
        )
}

fn click_at(harness: &mut App, app: &mut ShowcaseApp, position: Pos2) -> Vec<egui::FullOutput> {
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
    .map(|events| harness.step(app, events))
    .collect()
}

fn opened_url(outputs: &[egui::FullOutput]) -> Option<String> {
    outputs
        .iter()
        .flat_map(|output| &output.platform_output.commands)
        .find_map(|command| match command {
            egui::OutputCommand::OpenUrl(url) => Some(url.url.clone()),
            _ => None,
        })
}

fn overview_text(lang: Language) -> String {
    let mut app = ShowcaseApp::default();
    rendered_text(|ui| {
        _ = app.render_component(ui, lang);
    })
}

#[test]
fn the_home_page_shows_three_localized_primary_actions() {
    for (lang, source, donate, old_intro) in [
        (
            Language::Eng,
            BaseMsg::SourceCodeButton.render(Language::Eng),
            BaseMsg::Donate.render(Language::Eng),
            "Explore functora-egui: ",
        ),
        (
            Language::Spa,
            BaseMsg::SourceCodeButton.render(Language::Spa),
            BaseMsg::Donate.render(Language::Spa),
            "Explora functora-egui: ",
        ),
        (
            Language::Rus,
            BaseMsg::SourceCodeButton.render(Language::Rus),
            BaseMsg::Donate.render(Language::Rus),
            "Изучите functora-egui: ",
        ),
    ] {
        let text = overview_text(lang);
        for label in [source, "crates.io".to_owned(), donate] {
            assert_eq!(
                text.matches(&label).count(),
                1,
                "the home page must show one {label:?} action in {lang:?}: {text}"
            );
        }
        assert!(
            !text.contains(old_intro),
            "the home page must not show the old links message in {lang:?}: {text}"
        );
    }
}

#[test]
fn the_source_action_opens_the_library_repository() {
    let mut app = ShowcaseApp::default();
    let mut harness = App::new();
    let output = harness.step(&mut app, vec![]);
    let position = text_position(&output, &BaseMsg::SourceCodeButton.render(Language::Eng));
    let outputs = click_at(&mut harness, &mut app, position);
    assert_eq!(opened_url(&outputs).as_deref(), Some(LIBRARY_SOURCE_URL));
}

#[test]
fn the_crates_action_opens_the_library_crates_page() {
    let mut app = ShowcaseApp::default();
    let mut harness = App::new();
    let output = harness.step(&mut app, vec![]);
    let position = text_position(&output, "crates.io");
    let outputs = click_at(&mut harness, &mut app, position);
    assert_eq!(opened_url(&outputs).as_deref(), Some(LIBRARY_CRATES_URL));
}

#[test]
fn the_donate_action_opens_the_donate_screen() {
    let mut app = ShowcaseApp::default();
    let mut harness = App::new();
    let output = harness.step(&mut app, vec![]);
    let position = text_position(&output, &BaseMsg::Donate.render(Language::Eng));
    let _ = click_at(&mut harness, &mut app, position);
    assert_eq!(app.router.current().clone(), AppRoute::Donate);
}

#[test]
fn the_home_page_points_at_the_live_library_pages() {
    assert_eq!(
        LIBRARY_SOURCE_URL,
        "https://github.com/functora/functora.github.io/tree/master/rust/functora-egui"
    );
    assert_eq!(LIBRARY_CRATES_URL, "https://crates.io/crates/functora-egui");
}

#[test]
fn the_home_page_lists_every_category() {
    let mut app = ShowcaseApp::default();
    let text = rendered_text_sized(
        &mut |ui| {
            _ = app.render_component(ui, Language::Eng);
        },
        4000.0,
    );
    for (category, _, _) in CATEGORIES {
        if *category == CategoryId::Overview {
            continue;
        }
        assert!(
            text.contains(&category.render(Language::Eng)),
            "the home page must list the {category:?} category: {text}"
        );
    }
}
