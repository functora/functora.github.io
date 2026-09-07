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
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let count = self.item_count;
        if count == 0 {
            ui.allocate_response(egui::vec2(0.0, 0.0), egui::Sense::hover())
        } else {
            *current = (*current).min(count.saturating_sub(1));
            let last = count.saturating_sub(1);
            let dot_touch = spacing.touch_height;
            let dot_diameter: f32 = 8.0;
            ui.vertical(|inner_ui| {
                let frame = egui::Frame::NONE
                    .fill(egui::Color32::TRANSPARENT)
                    .inner_margin(egui::Margin::same(0));
                let _ = frame.show(inner_ui, |content_ui| {
                    content(content_ui, *current);
                });
                inner_ui.add_space(8.0);
                let _ = inner_ui.horizontal(|nav_ui| {
                    let prev_enabled = *current > 0;
                    let prev_btn = crate::widgets::button::widget::Button::icon_only(
                        crate::icons::lucide_icon::LucideIcon::ChevronLeft,
                    )
                    .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                    .size(crate::tokens::component_size::ComponentSize::Sm)
                    .enabled(prev_enabled);
                    let prev_resp = prev_btn.show(nav_ui);
                    if prev_resp.clicked() && prev_enabled {
                        *current = current.saturating_sub(1);
                        nav_ui.ctx().request_repaint();
                    }
                    (0..count).for_each(|idx| {
                        let (rect, resp) = nav_ui.allocate_exact_size(
                            egui::vec2(dot_touch, dot_touch),
                            egui::Sense::click(),
                        );
                        let color = if idx == *current {
                            theme.primary
                        } else {
                            theme.muted
                        };
                        if nav_ui.is_rect_visible(rect) {
                            let _ = nav_ui.painter().circle_filled(
                                rect.center(),
                                dot_diameter / 2.0,
                                color,
                            );
                        }
                        if resp.clicked() && idx != *current {
                            *current = idx;
                            nav_ui.ctx().request_repaint();
                        }
                        if resp.hovered() {
                            nav_ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                        }
                    });
                    let next_enabled = *current < last;
                    let next_btn = crate::widgets::button::widget::Button::icon_only(
                        crate::icons::lucide_icon::LucideIcon::ChevronRight,
                    )
                    .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                    .size(crate::tokens::component_size::ComponentSize::Sm)
                    .enabled(next_enabled);
                    let next_resp = next_btn.show(nav_ui);
                    if next_resp.clicked() && next_enabled {
                        *current = (*current + 1).min(last);
                        nav_ui.ctx().request_repaint();
                    }
                });
            })
            .response
        }
    }
}
