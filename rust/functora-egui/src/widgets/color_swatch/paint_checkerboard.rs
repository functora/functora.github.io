//! Checkerboard painter for transparent color swatches.

pub(crate) fn paint_checkerboard(painter: &egui::Painter, rect: egui::Rect, cell: f32) {
    let light = egui::Color32::from_gray(210);
    let dark = egui::Color32::from_gray(150);
    let cols = crate::utils::f32_to_usize_clamped((rect.width() / cell).ceil());
    let rows = crate::utils::f32_to_usize_clamped((rect.height() / cell).ceil());

    for row in 0..rows {
        for col in 0..cols {
            let min = egui::pos2(
                rect.min.x + crate::utils::usize_to_f32(col) * cell,
                rect.min.y + crate::utils::usize_to_f32(row) * cell,
            );
            let max = egui::pos2(
                (min.x + cell).min(rect.max.x),
                (min.y + cell).min(rect.max.y),
            );
            let color = if (row + col) % 2 == 0 { light } else { dark };
            let _ = painter.rect_filled(egui::Rect::from_min_max(min, max), 0.0, color);
        }
    }
}
