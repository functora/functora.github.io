//! The desktop-only quit entry of the catalog sidebar: it is the last entry,
//! it closes the window and it leaves the current screen alone.

use egui::{Event, PointerButton, Pos2, RawInput, Rect, Vec2};
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::white_label::QUIT_AVAILABLE;
use functora_egui_demo::ShowcaseApp;

const VIEWPORT: Vec2 = Vec2::new(1280.0, 8000.0);

fn run(ctx: &egui::Context, app: &mut ShowcaseApp, events: Vec<Event>) -> egui::FullOutput {
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, VIEWPORT)),
        time: Some(1.0 / 60.0),
        events,
        ..Default::default()
    };
    let mut frame = eframe::Frame::_new_kittest();
    let mut output = ctx.run_ui(raw, |ui| {
        eframe::App::ui(app, ui, &mut frame);
    });
    output.textures_delta.clear();
    output
}

fn collect_texts(shape: &egui::Shape, out: &mut Vec<(String, Rect)>) {
    match shape {
        egui::Shape::Text(text) => out.push((
            text.galley.text().to_owned(),
            text.galley.rect.translate(text.pos.to_vec2()),
        )),
        egui::Shape::Vec(shapes) => shapes.iter().for_each(|inner| collect_texts(inner, out)),
        _ => {}
    }
}

fn sidebar_texts(output: &egui::FullOutput) -> Vec<(String, Rect)> {
    let mut found = Vec::new();
    for clipped in &output.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
        .into_iter()
        .filter(|(_, rect)| rect.min.x > 900.0)
        .collect()
}

fn english_app() -> ShowcaseApp {
    let mut app = ShowcaseApp::default();
    app.persistent.language = Language::Eng;
    app
}

fn closes_window(output: &egui::FullOutput) -> bool {
    output
        .viewport_output
        .get(&egui::ViewportId::ROOT)
        .is_some_and(|viewport| viewport.commands.contains(&egui::ViewportCommand::Close))
}

#[test]
fn quit_entry_follows_platform_availability() {
    let mut app = english_app();
    let ctx = egui::Context::default();
    let output = run(&ctx, &mut app, vec![]);
    let quit = BaseMsg::Quit.render(Language::Eng);
    let rendered = sidebar_texts(&output).iter().any(|(text, _)| text == &quit);
    assert_eq!(
        rendered, QUIT_AVAILABLE,
        "the quit entry must be rendered exactly where it is available"
    );
}

#[test]
fn clicking_quit_closes_the_window_without_navigating() {
    let mut app = english_app();
    let ctx = egui::Context::default();
    let output = run(&ctx, &mut app, vec![]);
    let quit = BaseMsg::Quit.render(Language::Eng);
    let position = sidebar_texts(&output)
        .iter()
        .find(|(text, _)| text == &quit)
        .map_or_else(
            || panic!("the quit entry must render in the sidebar"),
            |(_, rect)| rect.center(),
        );
    let press = |pressed: bool| Event::PointerButton {
        pos: position,
        button: PointerButton::Primary,
        pressed,
        modifiers: egui::Modifiers::default(),
    };
    let outputs = [
        run(&ctx, &mut app, vec![Event::PointerMoved(position)]),
        run(&ctx, &mut app, vec![press(true)]),
        run(&ctx, &mut app, vec![press(false)]),
        run(&ctx, &mut app, vec![]),
    ];
    assert!(
        outputs.iter().any(closes_window),
        "clicking quit must ask the window to close"
    );
    assert_eq!(app.selected, None, "quitting must not navigate");
}
