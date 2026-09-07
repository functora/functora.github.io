#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Pagination layout regression: the bar must fit the available width on
//! mobile viewports instead of overflowing horizontally.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
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

fn assert_fits(screen_width: f32, total: usize, max_visible: usize, inner_width: Option<f32>) {
    let screen = Vec2::new(screen_width, 800.0);
    for page in 0..total.max(1) {
        let mut app = Harness::new();
        let page_cell = Rc::new(Cell::new(page));
        let width_cell = Rc::new(Cell::new(0.0_f32));
        let avail_cell = Rc::new(Cell::new(0.0_f32));
        let mut body = {
            let page_inner = Rc::clone(&page_cell);
            let width_inner = Rc::clone(&width_cell);
            let avail_inner = Rc::clone(&avail_cell);
            move |ui: &mut egui::Ui| {
                let mut current = page_inner.get();
                if let Some(max) = inner_width {
                    let _ = ui.vertical(|col| {
                        col.set_max_width(max);
                        avail_inner.set(col.available_width());
                        let resp = functora_egui::Pagination::new(total)
                            .max_visible(max_visible)
                            .show(col, &mut current);
                        width_inner.set(resp.rect.width());
                    });
                } else {
                    avail_inner.set(ui.available_width());
                    let resp = functora_egui::Pagination::new(total)
                        .max_visible(max_visible)
                        .show(ui, &mut current);
                    width_inner.set(resp.rect.width());
                }
                page_inner.set(current);
            }
        };
        let _ = app.step(screen, &mut body);
        drop(body);
        let bar = width_cell.get();
        let avail = avail_cell.get();
        assert!(
            bar <= avail + 1.0,
            "pagination w={bar:.1} overflows avail={avail:.1} (screen={screen_width}, total={total}, max_visible={max_visible}, page={page}, inner={inner_width:?})"
        );
    }
}

#[test]
fn pagination_fits_on_mobile() {
    assert_fits(390.0, 20, 5, None);
}

#[test]
fn pagination_fits_in_narrow_shell_column() {
    assert_fits(390.0, 20, 5, Some(342.0));
}

#[test]
fn pagination_fits_on_desktop() {
    assert_fits(1280.0, 20, 7, None);
}

#[test]
fn pagination_empty_does_not_panic() {
    let mut app = Harness::new();
    let page_cell = Rc::new(Cell::new(0usize));
    let mut body = {
        let page_inner = Rc::clone(&page_cell);
        move |ui: &mut egui::Ui| {
            let mut current = page_inner.get();
            let _ = functora_egui::Pagination::new(0).show(ui, &mut current);
            page_inner.set(current);
        }
    };
    let _ = app.step(Vec2::new(390.0, 800.0), &mut body);
    let _ = app.step(Vec2::new(1280.0, 800.0), &mut body);
}

#[test]
fn pagination_clamps_out_of_bounds_page() {
    let mut app = Harness::new();
    let page_cell = Rc::new(Cell::new(99usize));
    let mut body = {
        let page_inner = Rc::clone(&page_cell);
        move |ui: &mut egui::Ui| {
            let mut current = page_inner.get();
            let _ = functora_egui::Pagination::new(20)
                .max_visible(5)
                .show(ui, &mut current);
            page_inner.set(current);
        }
    };
    let _ = app.step(Vec2::new(390.0, 800.0), &mut body);
    drop(body);
    assert_eq!(page_cell.get(), 19);
}

#[test]
fn pagination_zero_max_visible_does_not_panic() {
    let mut app = Harness::new();
    let page_cell = Rc::new(Cell::new(10usize));
    let mut body = {
        let page_inner = Rc::clone(&page_cell);
        move |ui: &mut egui::Ui| {
            let mut current = page_inner.get();
            let _ = functora_egui::Pagination::new(20)
                .max_visible(0)
                .show(ui, &mut current);
            page_inner.set(current);
        }
    };
    let _ = app.step(Vec2::new(390.0, 800.0), &mut body);
    let _ = app.step(Vec2::new(1280.0, 800.0), &mut body);
}
