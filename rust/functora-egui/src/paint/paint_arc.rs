//! Arc painting utility for spinners.

/// Paints a 270° arc centered at `center` with given `radius` and `stroke`.
/// `angle_offset` rotates the arc start position (in radians).
pub fn paint_arc(
    painter: &egui::Painter,
    center: egui::Pos2,
    radius: f32,
    angle_offset: f32,
    stroke: egui::Stroke,
) {
    let segments = 32;
    let arc_angle = std::f32::consts::PI * 1.5; // 270°

    let points: Vec<egui::Pos2> = (0..=segments)
        .map(|i| {
            let t = crate::utils::i32_to_f32(i) / crate::utils::i32_to_f32(segments);
            let angle = angle_offset + t * arc_angle;
            egui::pos2(
                center.x + radius * angle.cos(),
                center.y + radius * angle.sin(),
            )
        })
        .collect();

    let _ = painter.add(egui::Shape::line(points, stroke));
}
