//! Every catalog screen must keep its text inside the viewport, whatever the
//! screen width: long titles wrap instead of painting past the shell.

use egui::{Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui_demo::{ComponentId, ShowcaseApp};

const WIDTHS: [f32; 4] = [320.0, 360.0, 400.0, 800.0];
const HEIGHT: f32 = 640.0;

fn collect_text(shape: &egui::Shape, clip: Rect, out: &mut Vec<(Rect, String)>) {
    match shape {
        Shape::Text(text) => {
            let painted = text
                .galley
                .rect
                .translate(text.pos.to_vec2())
                .intersect(clip);
            if painted.is_positive() {
                out.push((painted, text.galley.text().to_owned()));
            }
        }
        Shape::Vec(shapes) => {
            for inner in shapes {
                collect_text(inner, clip, out);
            }
        }
        _ => {}
    }
}

fn run_app(app: &mut ShowcaseApp, screen: Vec2) -> Vec<(Rect, String)> {
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
        ..Default::default()
    };
    let mut frame = eframe::Frame::_new_kittest();
    let mut out = ctx.run_ui(raw, |ui| {
        eframe::App::ui(app, ui, &mut frame);
    });
    out.textures_delta.clear();
    let mut rendered = Vec::new();
    for clipped in &out.shapes {
        collect_text(&clipped.shape, clipped.clip_rect, &mut rendered);
    }
    rendered
}

#[test]
fn catalog_text_stays_inside_the_viewport() {
    for width in WIDTHS {
        for id in ComponentId::ALL {
            let mut app = ShowcaseApp::default();
            app.navigate_to(Some(id));
            let rendered = run_app(&mut app, Vec2::new(width, HEIGHT));
            assert!(!rendered.is_empty(), "{id:?} must render text at {width}px");
            for (rect, text) in rendered {
                assert!(
                    rect.min.x >= -1.0 && rect.max.x <= width + 1.0,
                    "{id:?} paints {text:?} at {rect:?} outside a {width}px viewport"
                );
            }
        }
    }
}
