//! Show method for `ToggleGroup` — renders a set of exclusive toggles.

impl super::widget::ToggleGroup {
    /// Shows the toggle group. `selected` is the index of the active item.
    /// Returns the new selected index if changed.
    pub fn show(self, ui: &mut egui::Ui, selected: &mut usize) -> egui::Response {
        let entries: Vec<(usize, String)> = self.items.into_iter().enumerate().collect();
        let icons: Vec<(usize, crate::icons::lucide_icon::LucideIcon)> = self
            .icons
            .into_iter()
            .enumerate()
            .filter_map(|(idx, icon)| icon.map(|inner| (idx, inner)))
            .collect();
        super::widget::ToggleGroupValue::new(&entries)
            .icons(&icons)
            .variant(self.variant)
            .show(ui, selected)
    }

    fn render_item(
        ui: &mut egui::Ui,
        theme: &crate::theme::shadcn_theme::ShadcnTheme,
        label: &str,
        icon: Option<crate::icons::lucide_icon::LucideIcon>,
        is_selected: bool,
        cr: u8,
    ) -> egui::Response {
        let font_size: f32 = 13.0;
        let h_pad: f32 = 10.0;
        let height: f32 = 28.0;
        let icon_size: f32 = 14.0;
        let icon_gap: f32 = 6.0;
        let icon_w = if icon.is_some() {
            icon_size + icon_gap
        } else {
            0.0
        };

        let galley = ui.painter().layout_no_wrap(
            label.to_owned(),
            egui::FontId::proportional(font_size),
            theme.foreground,
        );

        let desired = egui::vec2(galley.size().x + h_pad * 2.0 + icon_w, height);
        let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let corner = egui::CornerRadius::same(cr.saturating_sub(1));

            let (bg, fg) = if is_selected {
                (theme.background, theme.foreground)
            } else if response.hovered() {
                (
                    egui::Color32::from_rgba_unmultiplied(
                        theme.background.r(),
                        theme.background.g(),
                        theme.background.b(),
                        128,
                    ),
                    theme.foreground,
                )
            } else {
                (egui::Color32::TRANSPARENT, theme.muted_foreground)
            };

            let _ = painter.rect_filled(rect, corner, bg);

            if is_selected {
                let _ = painter.rect_stroke(
                    rect,
                    corner,
                    egui::Stroke::new(
                        1.0,
                        egui::Color32::from_rgba_unmultiplied(
                            theme.foreground.r(),
                            theme.foreground.g(),
                            theme.foreground.b(),
                            13,
                        ),
                    ),
                    egui::epaint::StrokeKind::Inside,
                );
            }

            let content_x = rect.center().x - f32::midpoint(icon_w, galley.size().x);
            if let Some(icon_val) = icon {
                let icon_rect = egui::Rect::from_min_size(
                    egui::pos2(content_x, rect.center().y - icon_size / 2.0),
                    egui::vec2(icon_size, icon_size),
                );
                crate::icons::paint_icon::paint_icon(painter, icon_rect, &icon_val, fg);
            }
            let text_pos = egui::pos2(content_x + icon_w, rect.center().y - galley.size().y / 2.0);
            painter.galley(text_pos, galley, fg);
        }

        response
    }
}

impl<T: Clone + PartialEq> super::widget::ToggleGroupValue<'_, T> {
    /// Shows the toggle group. `selected` holds the active value.
    /// A value missing from `entries` keeps the current selection.
    pub fn show(self, ui: &mut egui::Ui, selected: &mut T) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = crate::utils::f32_to_u8_clamped(theme.radius);

        let outer_frame = egui::Frame::NONE
            .fill(theme.muted)
            .inner_margin(egui::Margin::same(2))
            .corner_radius(egui::CornerRadius::same(cr));

        outer_frame
            .show(ui, |inner_ui| {
                let _ = inner_ui.horizontal(|content_ui| {
                    content_ui.spacing_mut().item_spacing.x = 2.0;
                    for (value, label) in self.entries {
                        let is_selected = *value == *selected;
                        let icon = self
                            .icons
                            .iter()
                            .find(|entry| entry.0 == *value)
                            .map(|(_, icon)| *icon);
                        let response = super::widget::ToggleGroup::render_item(
                            content_ui,
                            &theme,
                            label,
                            icon,
                            is_selected,
                            cr,
                        );
                        if response.clicked() {
                            *selected = value.clone();
                            content_ui.ctx().request_repaint();
                        }
                    }
                });
            })
            .response
    }
}
