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

fn rects_of(shapes: &[ClippedShape]) -> Vec<Rect> {
    shapes
        .iter()
        .flat_map(|cs| match &cs.shape {
            Shape::Rect(rs) => vec![rs.rect],
            Shape::Mesh(m) => vec![m.calc_bounds()],
            Shape::Vec(v) => v
                .iter()
                .filter_map(|s| match s {
                    Shape::Rect(rs) => Some(rs.rect),
                    Shape::Mesh(m) => Some(m.calc_bounds()),
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

fn assert_no_horizontal_overflow(rects: &[Rect], screen: Vec2) {
    let screen_rect = Rect::from_min_size(Pos2::ZERO, screen);
    let overflow: Vec<Rect> = rects
        .iter()
        .filter(|r| r.min.x < -0.5 || r.max.x > screen_rect.max.x + 0.5)
        .copied()
        .collect();
    assert!(
        overflow.is_empty(),
        "screen overflows horizontally at {screen:?}: {overflow:?}"
    );
}

#[test]
fn share_screen_fits_iphone_se() {
    let ctx = Context::default();
    let cc = eframe::CreationContext::_new_kittest(ctx.clone());
    let mut app = CryptonoteApp::new(&cc);
    let screen = Vec2::new(375.0, 667.0);
    let mut shapes = run_frame(&ctx, &mut app, base_raw(screen));
    assert_no_horizontal_overflow(&rects_of(&shapes), screen);
    assert!(click_text(
        &ctx,
        &mut app,
        screen,
        &mut shapes,
        "AES-256-GCM"
    ));
    assert!(click_text(
        &ctx,
        &mut app,
        screen,
        &mut shapes,
        "No encryption"
    ));
    assert!(click_text(&ctx, &mut app, screen, &mut shapes, "Share"));
    let mut url_shown = false;
    for _ in 0..20 {
        shapes = run_frame(&ctx, &mut app, base_raw(screen));
        if texts_of(&shapes)
            .iter()
            .any(|(_, g)| g.text().contains("screen=open&note="))
        {
            url_shown = true;
            break;
        }
    }
    let texts = texts_of(&shapes);
    assert!(
        url_shown,
        "share screen must show the note URL, got texts: {:?}",
        texts.iter().map(|(_, g)| g.text()).collect::<Vec<_>>()
    );
    assert_no_horizontal_overflow(&rects_of(&shapes), screen);
}
