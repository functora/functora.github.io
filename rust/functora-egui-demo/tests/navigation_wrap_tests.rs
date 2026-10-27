//! Navigation must not corrupt layout: returning to the overview after
//! visiting a component screen keeps every section button single-line.
//! The flex containers reuse their ids across screens, so a widget that
//! sizes itself from the slot it is given would lock the other screen's
//! stale slot in as its own wrapped shape.

use egui::{Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui_demo::{ComponentId, ShowcaseApp};

const SCREENS: [Vec2; 2] = [Vec2::new(320.0, 640.0), Vec2::new(1280.0, 800.0)];
const LINE_HEIGHT: f32 = 22.0;

fn collect_texts(shape: &Shape, clip: Rect, out: &mut Vec<(Rect, String)>) {
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
                collect_texts(inner, clip, out);
            }
        }
        _ => {}
    }
}

fn rendered(out: &egui::FullOutput) -> Vec<(Rect, String)> {
    let mut found = Vec::new();
    for clipped in &out.shapes {
        collect_texts(&clipped.shape, clipped.clip_rect, &mut found);
    }
    found
}

fn step(
    app: &mut ShowcaseApp,
    ctx: &egui::Context,
    screen: Vec2,
    frame: &mut u32,
) -> egui::FullOutput {
    *frame += 1;
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
        time: Some(f64::from(*frame) / 60.0),
        ..Default::default()
    };
    let mut eframe_frame = eframe::Frame::_new_kittest();
    let mut out = ctx.run_ui(raw, |ui| {
        eframe::App::ui(app, ui, &mut eframe_frame);
    });
    out.textures_delta.clear();
    out
}

fn section_label_heights(out: &egui::FullOutput) -> Vec<(f32, String)> {
    let names: Vec<String> = ComponentId::ALL
        .iter()
        .map(|id| id.name().to_owned())
        .collect();
    rendered(out)
        .into_iter()
        .filter(|(_, text)| names.contains(text))
        .map(|(rect, text)| (rect.height(), text))
        .collect()
}

fn assert_section_labels_single_line(out: &egui::FullOutput, where_: &str) {
    let heights = section_label_heights(out);
    assert!(!heights.is_empty(), "section labels must render {where_}");
    for (height, text) in heights {
        assert!(
            height <= LINE_HEIGHT,
            "section label {text:?} wrapped to {height} tall {where_}"
        );
    }
}

#[test]
fn overview_buttons_stay_single_line_after_navigating_back() {
    for screen in SCREENS {
        let mut app = ShowcaseApp::default();
        let ctx = egui::Context::default();
        let mut frame = 0;
        for _ in 0..3 {
            let out = step(&mut app, &ctx, screen, &mut frame);
            assert_section_labels_single_line(&out, &format!("on the first {screen:?} overview"));
        }

        app.navigate_to(Some(ComponentId::Button));
        for _ in 0..3 {
            let _ = step(&mut app, &ctx, screen, &mut frame);
        }

        let _ = app.router.go_back();
        for frames_after in 0..4 {
            let out = step(&mut app, &ctx, screen, &mut frame);
            assert_section_labels_single_line(
                &out,
                &format!("{frames_after} frame(s) after navigating back at {screen:?}"),
            );
        }
    }
}
