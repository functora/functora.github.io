//! Snippet blocks must stay aligned with the code the demo renders live:
//! full entry lists, exhaustive enum matches, and the exact visible strings.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui_demo::ShowcaseApp;

fn rendered_text<F: FnMut(&mut egui::Ui)>(mut body: F) -> String {
    let ctx = Context::default();
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 900.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        },
    );
    out.textures_delta.clear();
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn rendered_app<F>(mut body: F) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let mut app = ShowcaseApp::default();
    rendered_text(|ui| body(&mut app, ui))
}

#[test]
fn enum_bound_snippets_list_every_live_entry() {
    let cases: Vec<(String, Vec<&str>)> = vec![
        (
            rendered_app(ShowcaseApp::demo_select),
            vec![
                "enum Fruit { Apple, Banana, Cherry, Grape, Mango }",
                "(Fruit::Mango, \"Mango\".to_owned())",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_select_value),
            vec![
                "enum BlendMode { Normal, Multiply, Screen, Overlay }",
                "(BlendMode::Overlay, \"Overlay\".to_owned())",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_combobox),
            vec![
                "enum Framework { React, Vue, Angular, Svelte, Solid }",
                "(Framework::Solid, \"Solid\".to_owned())",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_color_swatch),
            vec![
                "enum Swatch { Signal, Mint, Amber, Rose, Ink }",
                "(Swatch::Ink, \"Ink\", Color32::from_rgb(33, 37, 41))",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_radio_group),
            vec![
                "enum RadioOption { A, B, C }",
                "(RadioOption::C, \"Option C\".to_owned())",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_context_menu),
            vec![
                "enum ContextAction { Cut, Copy, Paste, SelectAll }",
                "(ContextAction::SelectAll, \"Select All\".to_owned())",
                "ContextAction::SelectAll => eprintln!(\"Select All\")",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_toolbar),
            vec![
                "enum Tool { Select, Pen, Spline, Frame, Text }",
                "(Tool::Text, LucideIcon::Type)",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_icon_tabs),
            vec![
                "LucideIcon::CircleUser, tooltip: \"Profile\".to_owned()",
                "LucideIcon::Bell, tooltip: \"Notifications\".to_owned()",
                "ProfileTab::Notifications => content.label(\"Notifications content\")",
            ],
        ),
        (
            rendered_app(ShowcaseApp::demo_field_group),
            vec!["(1..=12)", "(2030, \"2030\".to_owned())"],
        ),
        (
            rendered_app(ShowcaseApp::demo_property_row),
            vec!["(BlendMode::Overlay, \"Overlay\".to_owned())"],
        ),
        (
            rendered_app(ShowcaseApp::demo_menubar),
            vec![
                "enum EditAction { Undo, Redo, Cut, Copy, Paste }",
                "enum ViewAction { ZoomIn, ZoomOut, FullScreen }",
                "enum HelpAction { Documentation, About }",
                "(HelpAction::About, \"About\".to_owned())",
            ],
        ),
    ];

    for (text, expected) in cases {
        for needle in expected {
            assert!(
                text.contains(needle),
                "snippet must contain `{needle}` to match the live demo: {text}"
            );
        }
    }
}

#[test]
fn body_text_snippets_match_the_live_demo() {
    let cases = vec![
        (
            rendered_text(ShowcaseApp::demo_alert),
            "You can add components to your app using the CLI.",
        ),
        (
            rendered_app(ShowcaseApp::demo_dialog),
            "Tell us about yourself...",
        ),
        (
            rendered_text(ShowcaseApp::demo_hover_card),
            "Beautifully designed components that you can copy and paste into your apps.",
        ),
        (
            rendered_text(ShowcaseApp::demo_typography),
            "The Joke Tax Chronicles",
        ),
        (
            rendered_app(ShowcaseApp::demo_item),
            "(\"Appearance\", \"Choose a theme for the app\")",
        ),
        (
            rendered_text(ShowcaseApp::demo_empty),
            "Try adjusting your search to find what you're looking for.",
        ),
    ];

    for (text, needle) in cases {
        assert!(
            text.contains(needle),
            "snippet must render `{needle}` exactly as the live demo does: {text}"
        );
    }
}

#[test]
fn data_and_button_snippets_carry_the_full_lists() {
    let cases = vec![
        (
            rendered_text(ShowcaseApp::demo_table),
            "vec![\"Edsger Dijkstra\", \"Active\", \"Editor\"]",
        ),
        (
            rendered_app(ShowcaseApp::demo_carousel),
            "(\"Slide 4\", Color32::from_rgb(224, 49, 49))",
        ),
        (
            rendered_app(ShowcaseApp::demo_button),
            "Button::new(\"Open\").shortcut_text(\"Ctrl+O\")",
        ),
        (
            rendered_app(ShowcaseApp::demo_button),
            "ButtonGroup::show(ui, |g| {",
        ),
        (
            rendered_app(ShowcaseApp::demo_flex),
            "\"theming\", \"buttons\", \"inputs\", \"cards\", \"dialogs\", \"toasts\", \"badges\"",
        ),
    ];

    for (text, needle) in cases {
        assert!(
            text.contains(needle),
            "snippet must contain `{needle}` to match the live demo: {text}"
        );
    }
}

#[test]
fn nav_snippet_binds_typed_component_routes() {
    let text = rendered_app(ShowcaseApp::demo_nav);
    assert!(
        text.contains("AppRoute::Component(ComponentId::Button)"),
        "nav snippet must build typed ComponentId routes: {text}"
    );
    assert!(
        !text.contains("Component(42)"),
        "nav snippet must not use a blind integer route: {text}"
    );
}
