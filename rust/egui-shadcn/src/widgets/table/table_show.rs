//! Show method for Table — renders a styled data table.

impl super::table::Table {
    /// Shows the table. Returns the response for the outer frame.
    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = theme.radius.round() as u8;

        let frame = egui::Frame::NONE
            .fill(theme.card)
            .corner_radius(egui::CornerRadius::same(cr))
            .stroke(egui::Stroke::new(1.0, theme.border));

        frame
            .show(ui, |ui| {
                // Remove default spacing so rows pack tightly against dividers
                ui.spacing_mut().item_spacing = egui::vec2(0.0, 0.0);

                let col_count = self.headers.len();
                if col_count == 0 {
                    return;
                }

                let available = ui.available_width();
                let col_widths = compute_col_widths(available, col_count, &self.col_weights);

                // Header row
                Self::render_row(
                    ui,
                    &self.headers,
                    &col_widths,
                    available,
                    theme.muted,
                    theme.muted_foreground,
                    true,
                );

                // Divider after header
                Self::draw_hline(ui, available, &theme);

                // Data rows
                for (row_idx, row) in self.rows.iter().enumerate() {
                    let bg = if self.striped && row_idx % 2 == 1 {
                        theme.muted
                    } else {
                        egui::Color32::TRANSPARENT
                    };

                    Self::render_row(ui, row, &col_widths, available, bg, theme.foreground, false);

                    // Row divider (not after last)
                    if row_idx < self.rows.len() - 1 {
                        Self::draw_hline(ui, available, &theme);
                    }
                }
            })
            .response
    }

    fn draw_hline(ui: &mut egui::Ui, width: f32, theme: &crate::theme::shadcn_theme::ShadcnTheme) {
        let (line_rect, _) = ui.allocate_exact_size(egui::vec2(width, 1.0), egui::Sense::hover());
        ui.painter().hline(
            line_rect.x_range(),
            line_rect.center().y,
            egui::Stroke::new(1.0, theme.border),
        );
    }

    fn render_row(
        ui: &mut egui::Ui,
        cells: &[String],
        col_widths: &[f32],
        total_width: f32,
        bg: egui::Color32,
        fg: egui::Color32,
        is_header: bool,
    ) {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let base_height: f32 = if is_header { 40.0 } else { 44.0 };
        let font_size: f32 = if is_header { 13.0 } else { 14.0 };

        let galleys: Vec<_> = cells
            .iter()
            .enumerate()
            .map(|(idx, cell)| {
                let col_w = col_widths.get(idx).copied().unwrap_or(100.0);
                let wrap_w = (col_w - 24.0).max(40.0);
                ui.painter().layout(
                    cell.clone(),
                    egui::FontId::proportional(font_size),
                    fg,
                    wrap_w,
                )
            })
            .collect();
        let content_h = galleys.iter().map(|g| g.size().y).fold(0.0_f32, f32::max);
        let height = base_height.max(content_h + 16.0);

        let (row_rect, response) =
            ui.allocate_exact_size(egui::vec2(total_width, height), egui::Sense::hover());

        let actual_bg = if !is_header && response.hovered() {
            theme.accent
        } else {
            bg
        };

        if actual_bg != egui::Color32::TRANSPARENT {
            ui.painter()
                .rect_filled(row_rect, egui::CornerRadius::ZERO, actual_bg);
        }

        let mut x_offset = 0.0;
        for (col_idx, galley) in galleys.into_iter().enumerate() {
            let col_w = col_widths.get(col_idx).copied().unwrap_or(100.0);
            let text_pos = egui::pos2(
                row_rect.min.x + x_offset + 12.0,
                row_rect.center().y - galley.size().y / 2.0,
            );
            ui.painter().galley(text_pos, galley, fg);

            x_offset += col_w;
        }
    }
}

/// Computes column widths from optional weights, distributing total width proportionally.
fn compute_col_widths(total: f32, col_count: usize, weights: &Option<Vec<f32>>) -> Vec<f32> {
    match weights {
        Some(w) if w.len() == col_count => {
            let sum: f32 = w.iter().sum();
            if sum > 0.0 {
                w.iter().map(|&wt| total * wt / sum).collect()
            } else {
                vec![total / col_count as f32; col_count]
            }
        }
        _ => vec![total / col_count as f32; col_count],
    }
}
