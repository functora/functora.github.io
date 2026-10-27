//! Show method for `InputOtp` — renders individual digit input boxes.

impl super::widget::InputOtp {
    /// Shows the OTP input. `value` is a mutable string holding the entered digits.
    pub fn show(self, ui: &mut egui::Ui, value: &mut String) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let base_cell: f32 = 40.0;
        let gap: f32 = 8.0;
        let base_font_size: f32 = 20.0;
        let cells = crate::utils::usize_to_f32(self.length);
        let gaps = gap * crate::utils::usize_to_f32(self.length.saturating_sub(1));
        let cell_size = base_cell.min(((ui.available_width() - gaps) / cells).max(0.0));
        let font_size = base_font_size * (cell_size / base_cell);
        let cr = crate::paint::corner_radius(theme.radius);

        let total_width = cell_size * cells + gaps;
        let (full_rect, mut response) =
            ui.allocate_exact_size(egui::vec2(total_width, cell_size), egui::Sense::click());
        response.set_intrinsic_size(egui::vec2(base_cell * cells + gaps, base_cell));

        // Handle keyboard input when focused
        if response.has_focus() || response.clicked() {
            response.request_focus();

            let events: Vec<egui::Event> = ui.ctx().input(|i| i.events.clone());
            for event in &events {
                match event {
                    egui::Event::Text(t) => {
                        let room = self.length.saturating_sub(value.len());
                        value.extend(t.chars().filter(char::is_ascii_digit).take(room));
                        ui.ctx().request_repaint();
                    }
                    egui::Event::Key {
                        key: egui::Key::Backspace,
                        pressed: true,
                        ..
                    } => {
                        let _ = value.pop();
                        ui.ctx().request_repaint();
                    }
                    _ => {}
                }
            }
        }

        if response.has_focus() {
            ui.ctx().output_mut(|o| {
                o.ime = Some(egui::output::IMEOutput {
                    rect: full_rect,
                    cursor_rect: full_rect,
                    purpose: egui::IMEPurpose::Normal,
                    should_interrupt_composition: false,
                });
            });
        }

        if ui.is_rect_visible(full_rect) {
            let painter = ui.painter();
            let updated_digits: Vec<char> = value.chars().collect();

            (0..self.length).for_each(|i| {
                let x = full_rect.min.x + (cell_size + gap) * crate::utils::usize_to_f32(i);
                let cell_rect = egui::Rect::from_min_size(
                    egui::pos2(x, full_rect.min.y),
                    egui::vec2(cell_size, cell_size),
                );

                let is_active = i == updated_digits.len() && response.has_focus();
                let border_color = if is_active { theme.ring } else { theme.input };

                let _ = painter.rect_filled(cell_rect, cr, theme.background);
                let _ = painter.rect_stroke(
                    cell_rect,
                    cr,
                    egui::Stroke::new(if is_active { 2.0 } else { 1.0 }, border_color),
                    egui::epaint::StrokeKind::Inside,
                );

                if let Some(&ch) = updated_digits.get(i) {
                    let galley = crate::text::layout(
                        ui,
                        ch.to_string(),
                        egui::FontId::proportional(font_size),
                        theme.foreground,
                        (cell_size - 8.0).max(0.0),
                        Some(crate::text::Wrap::Truncate),
                    );
                    let pos = egui::pos2(
                        cell_rect.center().x - galley.size().x / 2.0,
                        cell_rect.center().y - galley.size().y / 2.0,
                    );
                    painter.galley(pos, galley, theme.foreground);
                }
            });

            // Focus ring
            if response.has_focus() {
                crate::paint::paint_focus_ring::paint_focus_ring(
                    painter,
                    full_rect,
                    theme.radius,
                    theme.ring,
                );
            }
        }

        response
    }
}
