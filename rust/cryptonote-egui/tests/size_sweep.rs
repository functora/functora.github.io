use cryptonote_egui::CryptonoteApp;
use eframe::App;
use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

fn run_app_frames(ctx: &Context, app: &mut CryptonoteApp, screen: Vec2) -> Vec<Rect> {
    let mut frame = eframe::Frame::_new_kittest();
    let mut all_rects: Vec<Rect> = Vec::new();
    for _ in 0..3 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
            ..Default::default()
        };
        let mut out = ctx.run_ui(raw, |ui| {
            app.ui(ui, &mut frame);
        });
        all_rects.extend(collect_rects(&out.shapes));
        out.textures_delta.clear();
    }
    all_rects
}

#[test]
fn real_app_fits_all_screen_sizes() {
    let sizes = [
        Vec2::new(320.0, 568.0),
        Vec2::new(360.0, 640.0),
        Vec2::new(375.0, 667.0),
        Vec2::new(414.0, 896.0),
        Vec2::new(768.0, 1024.0),
        Vec2::new(800.0, 600.0),
        Vec2::new(820.0, 600.0),
        Vec2::new(960.0, 600.0),
        Vec2::new(1024.0, 768.0),
        Vec2::new(1200.0, 800.0),
    ];
    for screen in sizes {
        let ctx = Context::default();
        let cc = eframe::CreationContext::_new_kittest(ctx.clone());
        let mut app = CryptonoteApp::new(&cc);
        let rects = run_app_frames(&ctx, &mut app, screen);
        let screen_rect = Rect::from_min_size(Pos2::ZERO, screen);
        let overflow: Vec<Rect> = rects
            .iter()
            .filter(|r| r.min.x < -0.5 || r.max.x > screen_rect.max.x + 0.5)
            .copied()
            .collect();
        eprintln!(
            "{screen:?}: {} shapes, overflow {}",
            rects.len(),
            overflow.len()
        );
        for r in &overflow {
            eprintln!("  OVERFLOW: {r:?}");
        }
        assert!(
            overflow.is_empty(),
            "{screen:?}: content overflows horizontally: {overflow:?}"
        );
    }
}

fn collect_rects(shapes: &[egui::epaint::ClippedShape]) -> Vec<Rect> {
    shapes
        .iter()
        .flat_map(|cs| match &cs.shape {
            Shape::Rect(rs) => vec![rs.rect],
            Shape::Vec(v) => v
                .iter()
                .filter_map(|s| match s {
                    Shape::Rect(rs) => Some(rs.rect),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}
