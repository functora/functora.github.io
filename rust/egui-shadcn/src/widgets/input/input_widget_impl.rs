//! Widget trait implementation for Input.

impl egui::Widget for super::input::Input<'_> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let height: f32 = 32.0; // h-8
        let h_padding: f32 = 10.0; // px-2.5
        let v_padding: f32 = 4.0; // py-1
        let width = self.desired_width.unwrap_or(ui.available_width());
        let reveal_size: f32 = 24.0;
        let reveal_id = ui.id().with("password_reveal");
        let revealed = if self.password {
            ui.data(|d| d.get_temp::<bool>(reveal_id)).unwrap_or(false)
        } else {
            false
        };

        let style = super::input_style::resolve_input_style(&theme, false);
        let cr = egui::CornerRadius::same(style.corner_radius.round() as u8);

        let desired = egui::vec2(width, height);
        let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::hover());
        let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);
        let bg = if outer_hovered {
            crate::paint::interpolate_color::interpolate_color(style.bg, theme.accent, 0.35)
        } else {
            style.bg
        };
        let border_color = if outer_hovered {
            theme.input
        } else {
            style.border_color
        };

        // Paint background and border
        ui.painter().rect_filled(outer_rect, cr, bg);
        ui.painter().rect_stroke(
            outer_rect,
            cr,
            egui::Stroke::new(1.0, border_color),
            egui::epaint::StrokeKind::Inside,
        );

        // Reserve space on the right for the reveal toggle
        let text_width = if self.password {
            inner_width(outer_rect, h_padding) - reveal_size - 6.0
        } else {
            inner_width(outer_rect, h_padding)
        };

        // Place the TextEdit inside, with padding
        let inner_rect = egui::Rect::from_min_max(
            egui::pos2(outer_rect.min.x + h_padding, outer_rect.min.y + v_padding),
            egui::pos2(
                outer_rect.min.x + h_padding + text_width,
                outer_rect.max.y - v_padding,
            ),
        );
        let mut child_ui = ui.new_child(
            egui::UiBuilder::new()
                .max_rect(inner_rect)
                .layout(egui::Layout::left_to_right(egui::Align::Center)),
        );

        let text_edit = egui::TextEdit::singleline(self.text)
            .frame(egui::Frame::NONE)
            .password(self.password && !revealed)
            .hint_text(&self.placeholder)
            .text_color(style.text_color)
            .desired_width(inner_rect.width());

        let response = child_ui.add(text_edit);

        // Reveal toggle for password inputs
        if self.password {
            let toggle_rect = egui::Rect::from_center_size(
                egui::pos2(
                    outer_rect.max.x - h_padding - reveal_size / 2.0,
                    outer_rect.center().y,
                ),
                egui::vec2(reveal_size, reveal_size),
            );
            let toggle_resp = ui.interact(
                toggle_rect,
                ui.id().with("reveal_toggle"),
                egui::Sense::click(),
            );
            if ui.is_rect_visible(toggle_rect) {
                crate::icons::paint_icon::paint_icon(
                    ui.painter(),
                    toggle_rect,
                    if revealed {
                        &crate::icons::lucide_icon::LucideIcon::EyeOff
                    } else {
                        &crate::icons::lucide_icon::LucideIcon::Eye
                    },
                    if toggle_resp.hovered() {
                        theme.foreground
                    } else {
                        theme.muted_foreground
                    },
                );
            }
            if toggle_resp.clicked() {
                ui.data_mut(|d| d.insert_temp(reveal_id, !revealed));
            }
        }

        // Repaint border if focused
        if response.has_focus() {
            let focused_style = super::input_style::resolve_input_style(&theme, true);
            ui.painter().rect_stroke(
                outer_rect,
                cr,
                egui::Stroke::new(1.0, focused_style.border_color),
                egui::epaint::StrokeKind::Inside,
            );
            crate::paint::paint_focus_ring::paint_focus_ring(
                ui.painter(),
                outer_rect,
                style.corner_radius,
                theme.ring,
            );
        }

        response
    }
}

fn inner_width(outer_rect: egui::Rect, h_padding: f32) -> f32 {
    outer_rect.width() - h_padding * 2.0
}
