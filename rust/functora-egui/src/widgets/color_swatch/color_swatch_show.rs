//! Show method for `ColorSwatch`.

impl super::widget::ColorSwatch {
    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}

impl egui::Widget for super::widget::ColorSwatch {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let label_galley = self.label.as_ref().map(|label| {
            ui.painter().layout_no_wrap(
                label.clone(),
                egui::FontId::proportional(13.0),
                theme.foreground,
            )
        });
        let hex = format!(
            "#{:02X}{:02X}{:02X}",
            self.color.r(),
            self.color.g(),
            self.color.b()
        );
        let hex_galley = self.show_hex.then(|| {
            ui.painter().layout_no_wrap(
                hex,
                egui::FontId::proportional(11.0),
                theme.muted_foreground,
            )
        });

        let gap = if label_galley.is_some() || hex_galley.is_some() {
            8.0
        } else {
            0.0
        };
        let text_width = label_galley
            .as_ref()
            .map_or(0.0, |g| g.size().x)
            .max(hex_galley.as_ref().map_or(0.0, |g| g.size().x));
        let text_height = label_galley.as_ref().map_or(0.0, |g| g.size().y)
            + if label_galley.is_some() && hex_galley.is_some() {
                2.0
            } else {
                0.0
            }
            + hex_galley.as_ref().map_or(0.0, |g| g.size().y);

        let desired = egui::vec2(self.size + gap + text_width, self.size.max(text_height));
        let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let swatch_rect = egui::Rect::from_min_size(rect.min, egui::vec2(self.size, self.size));
            let radius =
                egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius * 0.75));

            if self.color.a() < 255 {
                super::paint_checkerboard::paint_checkerboard(painter, swatch_rect, 5.0);
            }

            let render_rect = if response.is_pointer_button_down_on() {
                swatch_rect.shrink(1.0)
            } else {
                swatch_rect
            };
            let _ = painter.rect_filled(render_rect, radius, self.color);

            let border = if response.hovered() || self.selected {
                theme.ring
            } else {
                theme.border
            };
            let _ = painter.rect_stroke(
                render_rect,
                radius,
                egui::Stroke::new(1.0, border),
                egui::epaint::StrokeKind::Inside,
            );

            if self.selected {
                let _ = painter.rect_stroke(
                    render_rect.expand(3.0),
                    egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius + 2.0)),
                    egui::Stroke::new(1.5, theme.primary),
                    egui::epaint::StrokeKind::Outside,
                );
            }

            let mut text_y = rect.center().y - text_height / 2.0;
            let text_x = render_rect.max.x + gap;
            if let Some(galley) = label_galley {
                painter.galley(egui::pos2(text_x, text_y), galley.clone(), theme.foreground);
                text_y += galley.size().y + 2.0;
            }
            if let Some(galley) = hex_galley {
                painter.galley(egui::pos2(text_x, text_y), galley, theme.muted_foreground);
            }
        }

        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        response
    }
}
