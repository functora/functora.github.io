//! Show method for Carousel -- renders a content slider with navigation.

impl super::widget::Carousel {
    /// Shows the carousel. `current` is the active slide index.
    /// `content(ui, index)` renders each slide.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        current: &mut usize,
        content: impl Fn(&mut egui::Ui, usize),
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let last = self.item_count.saturating_sub(1);

        ui.vertical(|inner_ui| {
            // Content area
            let frame = egui::Frame::NONE
                .fill(egui::Color32::TRANSPARENT)
                .inner_margin(egui::Margin::same(0));

            let _ = frame.show(inner_ui, |content_ui| {
                content(content_ui, *current);
            });

            inner_ui.add_space(8.0);

            // Navigation controls
            let _ = inner_ui.horizontal(|content_ui| {
                // Previous button
                let prev_enabled = *current > 0;
                let prev_btn = crate::widgets::button::widget::Button::icon_only(
                    crate::icons::lucide_icon::LucideIcon::ChevronLeft,
                )
                .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                .size(crate::tokens::component_size::ComponentSize::Sm);
                let prev_resp = prev_btn.show(content_ui);
                if prev_resp.clicked() && prev_enabled {
                    *current = current.saturating_sub(1);
                    content_ui.ctx().request_repaint();
                }

                // Dot indicators
                for i in 0..self.item_count {
                    let size: f32 = 8.0;
                    let (rect, _) = content_ui
                        .allocate_exact_size(egui::vec2(size, size), egui::Sense::hover());
                    let color = if i == *current {
                        theme.primary
                    } else {
                        theme.muted
                    };
                    let _ = content_ui
                        .painter()
                        .circle_filled(rect.center(), size / 2.0, color);
                }

                // Next button
                let next_enabled = *current < last;
                let next_btn = crate::widgets::button::widget::Button::icon_only(
                    crate::icons::lucide_icon::LucideIcon::ChevronRight,
                )
                .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                .size(crate::tokens::component_size::ComponentSize::Sm);
                let next_resp = next_btn.show(content_ui);
                if next_resp.clicked() && next_enabled {
                    *current = (*current + 1).min(last);
                    content_ui.ctx().request_repaint();
                }
            });
        })
        .response
    }
}
