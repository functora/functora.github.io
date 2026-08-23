//! Show method for Accordion -- renders collapsible sections with dividers.

impl super::widget::Accordion {
    /// Shows the accordion. `open_indices` tracks which sections are expanded.
    pub fn show(self, ui: &mut egui::Ui, open_indices: &mut Vec<usize>) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        ui.vertical(|inner_ui| {
            for (idx, (title, content)) in self.items.iter().enumerate() {
                let is_open = open_indices.contains(&idx);

                // Divider line (top border for each section)
                if idx > 0 {
                    let rect = inner_ui.available_rect_before_wrap();
                    let line_y = rect.min.y;
                    let _ = inner_ui.painter().hline(
                        rect.min.x..=rect.max.x,
                        line_y,
                        egui::Stroke::new(1.0, theme.border),
                    );
                }

                // Section header
                inner_ui.add_space(12.0);
                let header_response = inner_ui.horizontal(|content_ui| {
                    content_ui
                        .with_layout(
                            egui::Layout::left_to_right(egui::Align::Center),
                            |inner_ui3| {
                                let trigger = inner_ui3.add(
                                    egui::Label::new(
                                        egui::RichText::new(title)
                                            .color(theme.foreground)
                                            .size(14.0)
                                            .strong(),
                                    )
                                    .sense(egui::Sense::click()),
                                );

                                // Expand to fill width then add chevron on right
                                let icon_size: f32 = 14.0;
                                let remaining = inner_ui3.available_width() - icon_size - 4.0;
                                if remaining > 0.0 {
                                    inner_ui3.add_space(remaining);
                                }

                                let chevron_icon = if is_open {
                                    crate::icons::lucide_icon::LucideIcon::Minus
                                } else {
                                    crate::icons::lucide_icon::LucideIcon::Plus
                                };
                                let (icon_rect, _) = inner_ui3.allocate_exact_size(
                                    egui::vec2(icon_size, icon_size),
                                    egui::Sense::hover(),
                                );
                                if inner_ui3.is_rect_visible(icon_rect) {
                                    crate::icons::paint_icon::paint_icon(
                                        inner_ui3.painter(),
                                        icon_rect,
                                        &chevron_icon,
                                        theme.muted_foreground,
                                    );
                                }

                                trigger
                            },
                        )
                        .inner
                });

                let trigger = header_response.inner;

                if trigger.clicked() {
                    if is_open {
                        open_indices.retain(|&i| i != idx);
                    } else {
                        if !self.multiple {
                            open_indices.clear();
                        }
                        open_indices.push(idx);
                    }
                    inner_ui.ctx().request_repaint();
                }

                if trigger.hovered() {
                    inner_ui
                        .ctx()
                        .set_cursor_icon(egui::CursorIcon::PointingHand);
                }

                // Content
                if is_open {
                    inner_ui.add_space(4.0);
                    let _ = inner_ui.label(
                        egui::RichText::new(content)
                            .color(theme.muted_foreground)
                            .size(14.0),
                    );
                }

                inner_ui.add_space(12.0);
            }
        })
        .response
    }
}
