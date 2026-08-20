use cryptonote_egui::CryptonoteApp;
use eframe::App;
use egui::epaint::ClippedShape;
use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2, ViewportId};

fn base_raw(screen: Vec2) -> RawInput {
    RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
        viewport_id: ViewportId::ROOT,
        ..Default::default()
    }
}

fn run_frame(ctx: &Context, app: &mut CryptonoteApp, raw: RawInput) -> Vec<ClippedShape> {
    let mut frame = eframe::Frame::_new_kittest();
    let mut out = ctx.run_ui(raw, |ui| {
        app.ui(ui, &mut frame);
    });
    let shapes = out.shapes.clone();
    out.textures_delta.clear();
    shapes
}

fn texts_of(shapes: &[ClippedShape]) -> Vec<(Pos2, egui::Galley)> {
    shapes
        .iter()
        .flat_map(|cs| match &cs.shape {
            Shape::Text(ts) => vec![(ts.pos, ts.galley.as_ref().clone())],
            Shape::Vec(v) => v
                .iter()
                .filter_map(|s| match s {
                    Shape::Text(ts) => Some((ts.pos, ts.galley.as_ref().clone())),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn text_rect(texts: &[(Pos2, egui::Galley)], needle: &str) -> Option<Rect> {
    texts
        .iter()
        .find(|(_, g)| g.text().contains(needle))
        .map(|(pos, g)| Rect::from_min_size(*pos, g.size()))
}

fn click(ctx: &Context, app: &mut CryptonoteApp, screen: Vec2, pos: Pos2) {
    let mut raw = base_raw(screen);
    raw.events.push(Event::PointerMoved(pos));
    raw.events.push(Event::PointerButton {
        pos,
        button: egui::PointerButton::Primary,
        pressed: true,
        modifiers: egui::Modifiers::default(),
    });
    let _ = run_frame(ctx, app, raw);
    let mut release = base_raw(screen);
    release.events.push(Event::PointerMoved(pos));
    release.events.push(Event::PointerButton {
        pos,
        button: egui::PointerButton::Primary,
        pressed: false,
        modifiers: egui::Modifiers::default(),
    });
    let _ = run_frame(ctx, app, release);
}

fn click_text(
    ctx: &Context,
    app: &mut CryptonoteApp,
    screen: Vec2,
    shapes: &mut Vec<ClippedShape>,
    needle: &str,
) -> bool {
    for _ in 0..4 {
        if let Some(r) = text_rect(&texts_of(shapes), needle) {
            click(ctx, app, screen, r.center());
            *shapes = run_frame(ctx, app, base_raw(screen));
            return true;
        }
        *shapes = run_frame(ctx, app, base_raw(screen));
    }
    false
}

fn has_text(shapes: &[ClippedShape], needle: &str) -> bool {
    texts_of(shapes)
        .iter()
        .any(|(_, g)| g.text().contains(needle))
}

#[test]
fn desktop_nav_has_language_menu_theme_and_brand() {
    let ctx = Context::default();
    let cc = eframe::CreationContext::_new_kittest(ctx.clone());
    let mut app = CryptonoteApp::new(&cc);
    let screen = Vec2::new(1000.0, 600.0);
    let mut shapes = run_frame(&ctx, &mut app, base_raw(screen));
    assert!(
        has_text(&shapes, "Cryptonote-egui"),
        "nav must show the brand"
    );
    assert!(
        has_text(&shapes, "English"),
        "nav must show the current language"
    );
    assert!(
        !has_text(&shapes, "Español"),
        "language menu must be closed"
    );
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "English"));
    assert!(
        has_text(&shapes, "Español") && has_text(&shapes, "Русский"),
        "language menu must list all languages, got {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "Español"));
    assert!(
        has_text(&shapes, "Español"),
        "nav must show the chosen language"
    );
}

#[test]
fn mobile_nav_collapses_into_drawer_with_same_items() {
    let ctx = Context::default();
    let cc = eframe::CreationContext::_new_kittest(ctx.clone());
    let mut app = CryptonoteApp::new(&cc);
    let screen = Vec2::new(375.0, 667.0);
    let mut shapes = run_frame(&ctx, &mut app, base_raw(screen));
    assert!(
        has_text(&shapes, "Cryptonote-egui"),
        "mobile nav must show the brand"
    );
    assert!(
        !has_text(&shapes, "Русский"),
        "collapsed nav must hide the language items"
    );
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "\u{E017}"));
    assert!(
        has_text(&shapes, "English")
            && has_text(&shapes, "Español")
            && has_text(&shapes, "Русский"),
        "drawer must list all languages, got {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
    assert!(
        has_text(&shapes, "Theme"),
        "drawer must show the theme item, got {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
    eprintln!(
        "before Russian click: {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "Русский"));
    for _ in 0..20 {
        shapes = run_frame(&ctx, &mut app, base_raw(screen));
    }
    eprintln!(
        "after close pump: {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
    assert!(
        !has_text(&shapes, "Español"),
        "drawer must close after choosing a language"
    );
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "\u{E017}"));
    assert!(
        has_text(&shapes, "✓ 🇷🇺 Русский"),
        "drawer must mark the current language, got {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
}

#[test]
fn brand_click_resets_to_home() {
    let ctx = Context::default();
    let cc = eframe::CreationContext::_new_kittest(ctx.clone());
    let mut app = CryptonoteApp::new(&cc);
    let screen = Vec2::new(1000.0, 600.0);
    let mut shapes = run_frame(&ctx, &mut app, base_raw(screen));
    assert!(click_text(
        &ctx,
        &mut app,
        screen,
        &mut shapes,
        "Cryptonote-egui"
    ));
    assert!(
        has_text(&shapes, "Create note"),
        "brand click must land on the home screen, got {:?}",
        texts_of(&shapes)
            .iter()
            .map(|(_, g)| g.text())
            .collect::<Vec<_>>()
    );
}
