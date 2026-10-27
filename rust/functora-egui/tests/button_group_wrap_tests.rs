//! A connected button group wraps onto several rows instead of pushing its
//! titles off the screen.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

fn titles() -> Vec<String> {
    [
        "Создать заметку",
        "Открыть заметку",
        "Сканировать заметку",
        "Настроить подпись",
        "Экспортировать архив",
    ]
    .iter()
    .map(ToString::to_string)
    .collect()
}

struct Harness {
    ctx: Context,
    frame: u32,
    screen: Vec2,
}

impl Harness {
    fn new(screen: Vec2) -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
            screen,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, self.screen)),
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

fn text_rects(out: &egui::FullOutput) -> Vec<(Rect, String)> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some((
                text.galley.rect.translate(text.pos.to_vec2()),
                text.galley.text().to_owned(),
            )),
            _ => None,
        })
        .collect()
}

fn group_body(titles: &[String], ui: &mut egui::Ui) {
    let _ = functora_egui::ButtonGroup::show(ui, |group_ui| {
        for title in titles {
            let _ = functora_egui::Button::new(title.clone()).show(group_ui);
        }
    });
}

#[test]
fn button_group_stays_on_screen_and_wraps_onto_several_rows() {
    for width in [320.0_f32, 360.0, 400.0] {
        let screen = Vec2::new(width, 640.0);
        let titles = titles();
        let mut app = Harness::new(screen);
        let mut body = |ui: &mut egui::Ui| group_body(&titles, ui);

        // The first frame caches the row structure the buttons paint with.
        let _ = app.step(&mut body);
        let out = app.step(&mut body);

        let rendered = text_rects(&out);
        assert_eq!(
            rendered.len(),
            titles.len(),
            "every title must render at width {width}"
        );

        let mut rows: Vec<f32> = Vec::new();
        for (rect, text) in &rendered {
            assert!(
                rect.min.x >= -1.0 && rect.max.x <= width + 1.0,
                "title {text:?} at {rect:?} escapes a {width}px viewport"
            );
            assert!(
                rect.width() <= width,
                "title {text:?} is wider than the group"
            );
            if !rows.iter().any(|y| (y - rect.min.y).abs() < 2.0) {
                rows.push(rect.min.y);
            }
        }
        assert!(
            rows.len() >= 2,
            "titles must wrap onto several rows at {width}px, got {} row(s)",
            rows.len()
        );
    }
}

#[test]
fn wrapping_is_stable_across_frames() {
    let titles = titles();
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut body = |ui: &mut egui::Ui| group_body(&titles, ui);
    let _ = app.step(&mut body);
    let first = text_rects(&app.step(&mut body));
    let second = text_rects(&app.step(&mut body));

    let positions = |rendered: &[(Rect, String)]| {
        rendered
            .iter()
            .map(|(rect, _)| (rect.min.x.round(), rect.min.y.round()))
            .collect::<Vec<_>>()
    };
    assert_eq!(
        positions(&first),
        positions(&second),
        "the cached rows must not move the buttons"
    );
}
