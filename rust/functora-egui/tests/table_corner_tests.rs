//! Table corner regression: opaque row fills must respect the outer frame's
//! rounded corners instead of painting square over the border.

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

fn muted_fills(out: &egui::FullOutput, muted: egui::Color32) -> Vec<(Rect, egui::CornerRadius)> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect_shape) if rect_shape.fill == muted => {
                Some((rect_shape.rect, rect_shape.corner_radius))
            }
            _ => None,
        })
        .collect()
}

fn render_striped_table(ui: &mut egui::Ui, muted_cell: &Rc<Cell<egui::Color32>>) {
    muted_cell.set(functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx()).muted);
    let headers = vec!["Name".to_owned(), "Status".to_owned(), "Role".to_owned()];
    let rows = vec![
        vec![
            "Ada Lovelace".to_owned(),
            "Active".to_owned(),
            "Admin".to_owned(),
        ],
        vec![
            "Alan Turing".to_owned(),
            "Active".to_owned(),
            "Editor".to_owned(),
        ],
        vec![
            "Grace Hopper".to_owned(),
            "Inactive".to_owned(),
            "Viewer".to_owned(),
        ],
        vec![
            "Edsger Dijkstra".to_owned(),
            "Active".to_owned(),
            "Editor".to_owned(),
        ],
    ];
    let _ = functora_egui::Table::new(headers)
        .rows(rows)
        .striped()
        .show(ui);
}

fn assert_rounded_corners(screen_width: f32) {
    let mut app = Harness::new();
    let muted_cell = Rc::new(Cell::new(egui::Color32::TRANSPARENT));
    let mut body = {
        let muted_inner = Rc::clone(&muted_cell);
        move |ui: &mut egui::Ui| render_striped_table(ui, &muted_inner)
    };
    let out = app.step(Vec2::new(screen_width, 800.0), &mut body);
    drop(body);
    let muted = muted_cell.get();
    let mut ordered = muted_fills(&out, muted);
    ordered.sort_by(|left, right| left.0.min.y.total_cmp(&right.0.min.y));
    if let [first, .., last] = ordered.as_slice() {
        assert!(
            first.0.min.y < last.0.min.y,
            "header and last row must be distinct rows"
        );
        assert!(
            first.1.nw > 0 && first.1.ne > 0 && first.1.sw == 0 && first.1.se == 0,
            "header fill must round the top corners, got {:?}",
            first.1
        );
        assert!(
            last.1.sw > 0 && last.1.se > 0 && last.1.nw == 0 && last.1.ne == 0,
            "last row fill must round the bottom corners, got {:?}",
            last.1
        );
        for (_, corners) in ordered.iter().skip(1).take(ordered.len().saturating_sub(2)) {
            assert!(
                *corners == egui::CornerRadius::ZERO,
                "middle row fill must stay square, got {corners:?}"
            );
        }
    } else {
        panic!("expected header plus striped rows, found none");
    }
}

#[test]
fn table_corners_are_rounded_on_desktop() {
    assert_rounded_corners(1280.0);
}

#[test]
fn table_corners_are_rounded_on_mobile() {
    assert_rounded_corners(390.0);
}
