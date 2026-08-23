//! Show method for `NavigationMenu` — renders a horizontal nav bar.

impl super::widget::NavigationMenu {
    /// Shows the navigation menu. `active` is the currently selected item index.
    /// Returns the index of the clicked item, if any.
    pub fn show(self, ui: &mut egui::Ui, active: &mut usize) -> Option<usize> {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let mut clicked = None;

        let _ = ui.horizontal(|inner_ui| {
            inner_ui.spacing_mut().item_spacing.x = 2.0;
            for (idx, label) in self.items.iter().enumerate() {
                let is_active = idx == *active;
                let font_size: f32 = 14.0;
                let h_pad: f32 = 12.0;
                let height = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(
                    inner_ui.ctx(),
                )
                .touch_height;

                let fg = if is_active {
                    theme.foreground
                } else {
                    theme.muted_foreground
                };

                let galley = inner_ui.painter().layout_no_wrap(
                    label.clone(),
                    egui::FontId::proportional(font_size),
                    fg,
                );

                let desired = egui::vec2(galley.size().x + h_pad * 2.0, height);
                let (rect, response) = inner_ui.allocate_exact_size(desired, egui::Sense::click());

                if inner_ui.is_rect_visible(rect) {
                    let cr =
                        egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));

                    if is_active {
                        let _ = inner_ui.painter().rect_filled(rect, cr, theme.accent);
                    } else if response.is_pointer_button_down_on() {
                        let _ = inner_ui.painter().rect_filled(
                            rect,
                            cr,
                            crate::paint::interpolate_color::interpolate_color(
                                theme.accent,
                                theme.primary,
                                0.12,
                            ),
                        );
                    } else if response.hovered() {
                        let _ = inner_ui.painter().rect_filled(rect, cr, theme.accent);
                    }

                    let text_pos = egui::pos2(
                        rect.center().x - galley.size().x / 2.0,
                        rect.center().y - galley.size().y / 2.0,
                    );
                    inner_ui.painter().galley(text_pos, galley, fg);
                }

                if response.clicked() {
                    *active = idx;
                    clicked = Some(idx);
                    inner_ui.ctx().request_repaint();
                }

                if response.hovered() {
                    inner_ui
                        .ctx()
                        .set_cursor_icon(egui::CursorIcon::PointingHand);
                }
            }
        });

        clicked
    }
}
