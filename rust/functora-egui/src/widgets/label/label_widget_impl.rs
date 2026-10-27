//! Widget trait implementation for Label.

impl egui::Widget for super::widget::Label {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let color = if self.muted {
            theme.muted_foreground
        } else {
            theme.foreground
        };

        let (font_size, fixed_height) = match self.size {
            Some(size) => {
                let (height, _padding, fs) = size.metrics_for(
                    &crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx()),
                );
                (fs, Some(height))
            }
            None => (14.0, None),
        };

        let galley = crate::text::layout(
            ui,
            self.text,
            egui::FontId::proportional(font_size),
            color,
            ui.available_width(),
            self.wrap,
        );

        let desired_width = galley.size().x;
        let desired_height = match fixed_height {
            Some(h) => crate::text::box_height(&galley, h),
            None => galley.size().y,
        };
        let (rect, mut response) = ui.allocate_exact_size(
            egui::vec2(desired_width, desired_height),
            egui::Sense::hover(),
        );
        let intrinsic_width = galley.intrinsic_size().x;
        let intrinsic_height = match fixed_height {
            Some(h) => crate::text::intrinsic_box_height(&galley, h).max(desired_height),
            None => galley.intrinsic_size().y.max(desired_height),
        };
        response.set_intrinsic_size(egui::vec2(intrinsic_width, intrinsic_height));

        if ui.is_rect_visible(rect) {
            // Center text vertically within the allocated rect (matches button centering)
            let text_pos = egui::pos2(rect.min.x, rect.center().y - galley.size().y / 2.0);
            ui.painter().galley(text_pos, galley, color);
        }

        response
    }
}
