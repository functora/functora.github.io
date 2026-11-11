use egui::{Context, Pos2, RawInput, Rect, Vec2};

pub fn rendered_text<F: FnMut(&mut egui::Ui)>(mut body: F) -> String {
    rendered_text_sized(&mut body, 900.0)
}

/// Renders the body in a viewport of the given height: egui culls the shapes
/// painted outside the viewport.
pub fn rendered_text_sized<F: FnMut(&mut egui::Ui)>(body: &mut F, height: f32) -> String {
    let ctx = Context::default();
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, height))),
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
