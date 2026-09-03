//! Show method for `DatePicker` — renders date input with calendar popup.

impl super::widget::DatePicker {
    /// Shows the date picker. `state` holds the selected date.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        state: &mut super::date_picker_state::DatePickerState,
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        // Initialize year/month if unset
        if state.year == 0 {
            state.year = 2026;
            state.month = 1;
        }

        let display_text = if state.is_set() {
            state.format()
        } else {
            self.placeholder.clone()
        };

        let text_color = if state.is_set() {
            theme.foreground
        } else {
            theme.muted_foreground
        };

        // Trigger button: calendar icon + text
        let icon_size: f32 = 14.0;
        let gap: f32 = 6.0;
        let h_padding: f32 = 8.0;
        let height = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx())
            .touch_height;
        let galley =
            ui.painter()
                .layout_no_wrap(display_text, egui::FontId::proportional(14.0), text_color);
        let desired = egui::vec2(icon_size + gap + galley.size().x + h_padding * 2.0, height);
        let (trigger_rect, btn_response) = ui.allocate_exact_size(desired, egui::Sense::click());

        if ui.is_rect_visible(trigger_rect) {
            if btn_response.hovered() || btn_response.is_pointer_button_down_on() {
                let bg = if btn_response.is_pointer_button_down_on() {
                    crate::paint::interpolate_color::interpolate_color(
                        theme.background,
                        theme.accent,
                        0.85,
                    )
                } else {
                    theme.accent
                };
                let _ = ui.painter().rect_filled(
                    trigger_rect,
                    egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius)),
                    bg,
                );
            }

            let icon_rect = egui::Rect::from_min_size(
                egui::pos2(
                    trigger_rect.min.x + h_padding,
                    trigger_rect.center().y - icon_size / 2.0,
                ),
                egui::vec2(icon_size, icon_size),
            );
            crate::icons::paint_icon::paint_icon(
                ui.painter(),
                icon_rect,
                &crate::icons::lucide_icon::LucideIcon::Calendar,
                theme.muted_foreground,
            );

            let text_pos = egui::pos2(
                trigger_rect.min.x + h_padding + icon_size + gap,
                trigger_rect.center().y - galley.size().y / 2.0,
            );
            ui.painter().galley(text_pos, galley, text_color);
        }

        if btn_response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        let popup_id = btn_response.id.with("date_picker_popup");

        let toggle_cmd = if btn_response.clicked() {
            Some(egui::SetOpenCommand::Toggle)
        } else {
            None
        };

        let cr = crate::utils::f32_to_u8_clamped(theme.radius + 2.0);
        let themed_frame = egui::Frame::NONE
            .fill(theme.popover)
            .inner_margin(egui::Margin::same(12))
            .corner_radius(egui::CornerRadius::same(cr))
            .stroke(egui::Stroke::new(1.0, theme.border))
            .shadow(egui::Shadow {
                offset: [0, 4],
                blur: 12,
                spread: 0,
                color: egui::Color32::from_black_alpha(8),
            });

        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let screen_w = ui.ctx().input(|i| i.viewport_rect().width());
        let is_mobile_narrow = spacing.is_mobile() && screen_w < 400.0;
        let popup_width = if spacing.is_mobile() {
            (screen_w - 16.0 - 24.0).clamp(280.0, 360.0)
        } else {
            308.0
        };

        let mut close_popup = false;

        if is_mobile_narrow {
            let thin: f32 = 8.0;
            let outer_w = (screen_w - 2.0 * thin).clamp(304.0, 384.0);
            let inner_w = outer_w - 24.0;
            let mut is_open = ui.data(|d| d.get_temp::<bool>(popup_id)).unwrap_or(false);
            if btn_response.clicked() {
                is_open = !is_open;
                let _ = ui.data_mut(|d| d.insert_temp(popup_id, is_open));
                ui.ctx().request_repaint();
            }
            if is_open {
                let area_resp = egui::Area::new(popup_id)
                    .fixed_pos(egui::pos2(thin, trigger_rect.max.y + 8.0))
                    .order(egui::Order::Foreground)
                    .show(ui.ctx(), |area_ui| {
                        let _ = egui::Frame::NONE
                            .fill(theme.popover)
                            .inner_margin(egui::Margin::same(12))
                            .corner_radius(egui::CornerRadius::same(cr))
                            .stroke(egui::Stroke::new(1.0, theme.border))
                            .shadow(egui::Shadow {
                                offset: [0, 4],
                                blur: 12,
                                spread: 0,
                                color: egui::Color32::from_black_alpha(8),
                            })
                            .show(area_ui, |frame_ui| {
                                frame_ui.set_min_width(inner_w);
                                frame_ui.set_max_width(inner_w);
                                let cal = crate::widgets::calendar::widget::Calendar::new();
                                if let Some(_day) = cal.show(
                                    frame_ui,
                                    &mut state.year,
                                    &mut state.month,
                                    &mut state.day,
                                ) {
                                    close_popup = true;
                                }
                            });
                    });
                let area_rect = area_resp.response.rect;
                if ui.ctx().input(|i| i.pointer.any_click())
                    && let Some(pos) = ui.ctx().input(|i| i.pointer.interact_pos())
                    && !area_rect.contains(pos)
                    && !trigger_rect.contains(pos)
                {
                    close_popup = true;
                }
                if close_popup {
                    is_open = false;
                    let _ = ui.data_mut(|d| d.insert_temp(popup_id, is_open));
                }
            }
            if close_popup {
                ui.ctx().request_repaint();
            }
        } else {
            let popup = egui::Popup::new(popup_id, ui.ctx().clone(), &btn_response, ui.layer_id())
                .open_memory(toggle_cmd)
                .close_behavior(egui::PopupCloseBehavior::CloseOnClickOutside)
                .align(if spacing.is_mobile() {
                    egui::RectAlign::BOTTOM
                } else {
                    egui::RectAlign::BOTTOM_START
                })
                .gap(8.0)
                .frame(themed_frame);

            let _ = popup.show(|popup_ui: &mut egui::Ui| {
                popup_ui.set_min_width(popup_width);
                popup_ui.set_max_width(popup_width);
                let _ = popup_ui.with_layout(
                    egui::Layout::top_down(egui::Align::Center),
                    |centered_ui| {
                        let cal = crate::widgets::calendar::widget::Calendar::new();
                        if let Some(_day) = cal.show(
                            centered_ui,
                            &mut state.year,
                            &mut state.month,
                            &mut state.day,
                        ) {
                            close_popup = true;
                        }
                    },
                );
            });
        }

        if close_popup {
            egui::Popup::close_id(ui.ctx(), popup_id);
            ui.ctx().request_repaint();
        }

        btn_response
    }
}
