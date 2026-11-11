//! The default white-label sidebar: the quit entry is the last entry, it
//! exists on desktop only and it closes the app gracefully.

use std::borrow::Cow;

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::LucideIcon;
use functora_egui::i18n::Language;
use functora_egui::route::RouteMetadata;
use functora_egui::white_label::{QUIT_AVAILABLE, SidebarAction, default_sidebar};

const DESKTOP: Vec2 = Vec2::new(1280.0, 800.0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum TestRoute {
    #[default]
    Home,
    About,
}

impl std::fmt::Display for TestRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Home => "Home",
            Self::About => "About",
        })
    }
}

impl std::str::FromStr for TestRoute {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "Home" => Ok(Self::Home),
            "About" => Ok(Self::About),
            other => Err(format!("unknown route: {other}")),
        }
    }
}

impl RouteMetadata for TestRoute {
    fn label(&self, _lang: Language) -> Cow<'static, str> {
        Cow::Borrowed(match self {
            Self::Home => "Home",
            Self::About => "About",
        })
    }

    fn parent(&self) -> Option<Self> {
        None
    }

    fn children(&self) -> Vec<Self> {
        Vec::new()
    }
}

fn render_sidebar(
    ctx: &Context,
    events: Vec<Event>,
) -> (egui::FullOutput, Option<SidebarAction<TestRoute>>) {
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, DESKTOP)),
        time: Some(1.0 / 60.0),
        events,
        ..Default::default()
    };
    let action = std::cell::RefCell::new(None);
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            *action.borrow_mut() = default_sidebar(
                inner,
                &[
                    (TestRoute::Home, LucideIcon::House),
                    (TestRoute::About, LucideIcon::Info),
                ],
                "Reset",
                &TestRoute::Home,
                Language::Eng,
            );
        });
    });
    out.textures_delta.clear();
    let picked = action.borrow().clone();
    (out, picked)
}

/// The labels the sidebar painted, in painting order.
fn painted_labels(out: &egui::FullOutput) -> Vec<String> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect()
}

/// The box of the painted label, used as the click target.
fn label_rect(out: &egui::FullOutput, label: &str) -> Option<Rect> {
    out.shapes.iter().find_map(|clipped| match &clipped.shape {
        Shape::Text(text) if text.galley.text() == label => {
            Some(Rect::from_min_size(text.pos, text.galley.size()))
        }
        _ => None,
    })
}

#[test]
fn the_quit_entry_is_the_last_sidebar_entry() {
    let ctx = Context::default();
    let (out, _) = render_sidebar(&ctx, Vec::new());
    let labels = painted_labels(&out);
    let expected = if QUIT_AVAILABLE {
        vec!["Home", "About", "Reset", "Quit"]
    } else {
        vec!["Home", "About", "Reset"]
    };
    assert_eq!(labels, expected, "the sidebar must end with the quit entry");
}

#[test]
fn the_quit_entry_exists_on_desktop_only() {
    let ctx = Context::default();
    let (out, _) = render_sidebar(&ctx, Vec::new());
    assert_eq!(
        painted_labels(&out).last().map(String::as_str) == Some("Quit"),
        QUIT_AVAILABLE,
        "the quit entry must exist exactly where the app can be closed"
    );
    assert_eq!(
        QUIT_AVAILABLE,
        !cfg!(any(target_arch = "wasm32", target_os = "android")),
        "only the desktop build may close the app window"
    );
}

#[test]
fn clicking_the_quit_entry_asks_the_app_to_close() {
    let ctx = Context::default();
    let (out, _) = render_sidebar(&ctx, Vec::new());
    let target = label_rect(&out, "Quit")
        .unwrap_or_else(|| panic!("the quit entry must be painted: {:?}", painted_labels(&out)));
    let center = target.center();

    let (_, moved) = render_sidebar(&ctx, vec![Event::PointerMoved(center)]);
    assert_eq!(
        moved, None,
        "moving over the quit entry must not close the app"
    );

    let (_, pressed) = render_sidebar(
        &ctx,
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
    );
    assert_eq!(
        pressed, None,
        "pressing the quit entry must not close the app"
    );

    let (_, released) = render_sidebar(
        &ctx,
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
    );
    assert_eq!(
        released,
        Some(SidebarAction::Quit),
        "releasing the quit entry must ask the app to close"
    );
}

#[test]
fn requesting_a_quit_closes_the_window_gracefully() {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, DESKTOP)),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        functora_egui::platform::request_quit(ui.ctx());
    });
    out.textures_delta.clear();
    let commands = out
        .viewport_output
        .get(&egui::ViewportId::ROOT)
        .map(|viewport| viewport.commands.clone())
        .unwrap_or_default();
    assert!(
        commands.contains(&egui::ViewportCommand::Close),
        "quitting must close the window, got {commands:?}"
    );
}
