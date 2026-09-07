#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Carousel layout regression: slide content must reserve its height so the
//! code snippet below never overlaps the widget on mobile or desktop widths.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use std::cell::Cell;
use std::rc::Rc;

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(&mut self, screen: Vec2, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

fn assert_no_overlap(screen_width: f32) {
    let screen = Vec2::new(screen_width, 800.0);
    let mut app = Harness::new();
    let idx_cell = Rc::new(Cell::new(0usize));
    let snippet_cell = Rc::new(Cell::new(0.0_f32));
    let bottom_cell = Rc::new(Cell::new(0.0_f32));
    let mut body = {
        let idx_inner = Rc::clone(&idx_cell);
        let snippet_inner = Rc::clone(&snippet_cell);
        let bottom_inner = Rc::clone(&bottom_cell);
        move |ui: &mut egui::Ui| {
            let colors = [
                egui::Color32::from_rgb(25, 113, 194),
                egui::Color32::from_rgb(18, 184, 134),
                egui::Color32::from_rgb(245, 159, 0),
                egui::Color32::from_rgb(224, 49, 49),
            ];
            let total = colors.len();
            let mut current = idx_inner.get();
            let resp =
                functora_egui::Carousel::new(total).show(ui, &mut current, |slide_ui, slide| {
                    let width = slide_ui.available_width().min(420.0);
                    let (rect, _) = slide_ui
                        .allocate_exact_size(egui::vec2(width, 200.0), egui::Sense::hover());
                    if slide_ui.is_rect_visible(rect) {
                        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(slide_ui.ctx());
                        let painter = slide_ui.painter();
                        let _ = painter.rect_filled(
                            rect,
                            egui::CornerRadius::from(theme.radius),
                            colors[slide],
                        );
                        let galley = painter.layout_no_wrap(
                            format!("Slide {}", slide + 1),
                            egui::FontId::proportional(24.0),
                            egui::Color32::WHITE,
                        );
                        painter.galley(
                            egui::pos2(
                                rect.center().x - galley.size().x / 2.0,
                                rect.center().y - galley.size().y / 2.0,
                            ),
                            galley,
                            egui::Color32::WHITE,
                        );
                    }
                });
            idx_inner.set(current);
            bottom_inner.set(resp.rect.bottom());
            let before = ui.min_rect().bottom();
            functora_egui::snippet(ui, "// Carousel snippet");
            snippet_inner.set(before + 24.0);
        }
    };
    let out = app.step(screen, &mut body);
    drop(body);
    let slide = out
        .shapes
        .iter()
        .filter_map(|cs| match &cs.shape {
            Shape::Rect(rs) => Some(rs.rect),
            Shape::Vec(v) => v.iter().find_map(|s| match s {
                Shape::Rect(rs) => Some(rs.rect),
                _ => None,
            }),
            _ => None,
        })
        .find(|r| (r.height() - 200.0).abs() < 2.0 && r.width() >= 200.0)
        .expect("200px slide rect not found");
    let bottom = bottom_cell.get();
    let top = snippet_cell.get();
    assert!(
        bottom >= slide.bottom() - 1.0,
        "carousel response {bottom:.1} should include slide bottom {:.1} at width {screen_width}",
        slide.bottom()
    );
    assert!(
        top >= bottom - 1.0,
        "snippet top {top:.1} overlaps carousel bottom {bottom:.1} at width {screen_width}"
    );
}

#[test]
fn carousel_does_not_overlap_snippet_on_mobile() {
    assert_no_overlap(390.0);
}

#[test]
fn carousel_does_not_overlap_snippet_on_desktop() {
    assert_no_overlap(1280.0);
}

#[test]
fn carousel_empty_does_not_panic() {
    let mut app = Harness::new();
    let idx_cell = Rc::new(Cell::new(0usize));
    let mut body = {
        let idx_inner = Rc::clone(&idx_cell);
        move |ui: &mut egui::Ui| {
            let mut current = idx_inner.get();
            let _ = functora_egui::Carousel::new(0).show(ui, &mut current, |_, _| {
                panic!("content must not run with zero items");
            });
            idx_inner.set(current);
        }
    };
    let _ = app.step(Vec2::new(390.0, 800.0), &mut body);
    let _ = app.step(Vec2::new(1280.0, 800.0), &mut body);
}

#[test]
fn carousel_clamps_out_of_bounds_index() {
    let mut app = Harness::new();
    let idx_cell = Rc::new(Cell::new(99usize));
    let mut body = {
        let idx_inner = Rc::clone(&idx_cell);
        move |ui: &mut egui::Ui| {
            let mut current = idx_inner.get();
            let _ = functora_egui::Carousel::new(4).show(ui, &mut current, |slide_ui, _| {
                let _ = slide_ui.allocate_exact_size(
                    egui::vec2(slide_ui.available_width().min(420.0), 200.0),
                    egui::Sense::hover(),
                );
            });
            idx_inner.set(current);
        }
    };
    let _ = app.step(Vec2::new(1280.0, 800.0), &mut body);
    drop(body);
    assert_eq!(idx_cell.get(), 3);
}
