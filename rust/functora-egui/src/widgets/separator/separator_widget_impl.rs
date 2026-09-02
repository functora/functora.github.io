//! Widget trait implementation for Separator.

impl egui::Widget for super::widget::Separator {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        if self.horizontal {
            let has_label = self.text.is_some() || self.icon.is_some();
            if has_label {
                let icon_size = 12.0;
                let icon_gap = 4.0;
                let gap = 8.0;
                let galley_opt = self.text.as_ref().map(|text| {
                    ui.painter().layout_no_wrap(
                        text.clone(),
                        egui::FontId::proportional(12.0),
                        theme.muted_foreground,
                    )
                });
                let text_w = galley_opt.as_ref().map_or(0.0, |g| g.size().x);
                let text_h = galley_opt.as_ref().map_or(0.0, |g| g.size().y);
                let has_icon = self.icon.is_some();
                let has_text = galley_opt.is_some();
                let label_w = text_w
                    + if has_icon {
                        icon_size + if has_text { icon_gap } else { 0.0 }
                    } else {
                        0.0
                    };
                let label_h = text_h.max(if has_icon { icon_size } else { 0.0 });
                let available_w = ui.available_width();
                let desired = egui::vec2(available_w, label_h + 8.0);
                let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::hover());

                if ui.is_rect_visible(rect) {
                    let painter = ui.painter();
                    let cy = rect.center().y;
                    let label_x = rect.center().x - label_w / 2.0;

                    if label_x - gap > rect.min.x {
                        let _ = painter.hline(
                            rect.min.x..=(label_x - gap),
                            cy,
                            egui::Stroke::new(1.0, theme.border),
                        );
                    }
                    if label_x + label_w + gap < rect.max.x {
                        let _ = painter.hline(
                            (label_x + label_w + gap)..=rect.max.x,
                            cy,
                            egui::Stroke::new(1.0, theme.border),
                        );
                    }
                    if let Some(icon) = &self.icon {
                        let icon_rect = egui::Rect::from_min_size(
                            egui::pos2(label_x, cy - icon_size / 2.0),
                            egui::vec2(icon_size, icon_size),
                        );
                        crate::icons::paint_icon::paint_icon(
                            painter,
                            icon_rect,
                            icon,
                            theme.muted_foreground,
                        );
                    }
                    if let Some(galley) = galley_opt {
                        let text_x = label_x + if has_icon { icon_size + icon_gap } else { 0.0 };
                        painter.galley(
                            egui::pos2(text_x, cy - text_h / 2.0),
                            galley,
                            theme.muted_foreground,
                        );
                    }
                }

                response
            } else {
                let desired = egui::vec2(ui.available_width(), 1.0);
                let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::hover());

                if ui.is_rect_visible(rect) {
                    let _ = ui.painter().hline(
                        rect.x_range(),
                        rect.center().y,
                        egui::Stroke::new(1.0, theme.border),
                    );
                }

                response
            }
        } else {
            // Use a modest height instead of available_height() to avoid inflating
            // the row height in horizontal layouts. The line is painted over the
            // full row via min_rect after all siblings are laid out.
            let height = ui.spacing().interact_size.y.max(16.0);
            let desired = egui::vec2(1.0, height);
            let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::hover());

            if ui.is_rect_visible(rect) {
                let _ = ui.painter().vline(
                    rect.center().x,
                    rect.y_range(),
                    egui::Stroke::new(1.0, theme.border),
                );
            }

            response
        }
    }
}
