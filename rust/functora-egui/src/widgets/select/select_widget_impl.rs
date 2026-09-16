//! Widget trait implementation for Select and `SelectValue`.

impl<T: Clone + std::fmt::Display + PartialEq + 'static> egui::Widget
    for super::widget::Select<'_, T>
{
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let entries: Vec<(T, String)> = self
            .options
            .iter()
            .map(|option| (option.clone(), option.to_string()))
            .collect();
        super::widget::SelectLabeled::show_labeled(
            ui,
            self.selected,
            &entries,
            &self.placeholder,
            self.width,
            self.selected_text_override.as_deref(),
        )
    }
}

impl<T: Clone + PartialEq + 'static> egui::Widget for super::widget::SelectLabeled<'_, T> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        Self::show_labeled(
            ui,
            self.selected,
            self.entries,
            &self.placeholder,
            self.width,
            None,
        )
    }
}

impl<T: Clone + PartialEq + 'static> super::widget::SelectLabeled<'_, T> {
    fn show_labeled(
        ui: &mut egui::Ui,
        selected: &mut Option<T>,
        entries: &[(T, String)],
        placeholder: &str,
        width_opt: Option<f32>,
        text_override: Option<&str>,
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let style = super::select_style::resolve_select_style(&theme);
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let height = spacing.touch_height;
        let h_padding: f32 = spacing.touch_padding;
        let chevron_width: f32 = 20.0;
        let width = super::select_sizing::trigger_width(
            ui,
            width_opt,
            entries
                .iter()
                .map(|(_, label)| label.as_str())
                .chain([placeholder])
                .chain(text_override),
            h_padding,
            chevron_width,
            200.0,
        );
        let desired = egui::vec2(width, height);

        let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::click());
        let popup_id = response.id.with("popup");

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(style.corner_radius));
            let pressed = response.is_pointer_button_down_on();
            let trigger_bg = if pressed {
                crate::paint::interpolate_color::interpolate_color(
                    style.trigger_bg,
                    theme.accent,
                    0.85,
                )
            } else if response.hovered() {
                theme.accent
            } else {
                style.trigger_bg
            };
            let trigger_border = if response.hovered() || pressed {
                theme.ring
            } else {
                style.trigger_border
            };

            let _ = painter.rect_filled(rect, cr, trigger_bg);
            let _ = painter.rect_stroke(
                rect,
                cr,
                egui::Stroke::new(1.0, trigger_border),
                egui::epaint::StrokeKind::Inside,
            );

            // Display text — explicit override first, then the selected entry label.
            let display_text = if let Some(override_text) = text_override {
                override_text.to_owned()
            } else {
                match selected {
                    Some(val) => entries
                        .iter()
                        .find(|entry| entry.0 == *val)
                        .map_or_else(|| placeholder.to_owned(), |(_, label)| label.clone()),
                    None => placeholder.to_owned(),
                }
            };

            let text_color = if selected.is_some() || text_override.is_some() {
                style.trigger_text
            } else {
                theme.muted_foreground
            };

            let galley =
                painter.layout_no_wrap(display_text, egui::FontId::proportional(14.0), text_color);
            let text_pos = egui::pos2(
                rect.min.x + h_padding,
                rect.center().y - galley.size().y / 2.0,
            );
            let text_region = egui::Rect::from_min_max(
                egui::pos2(rect.min.x + h_padding, rect.min.y),
                egui::pos2(
                    (rect.max.x - h_padding - chevron_width).max(rect.min.x),
                    rect.max.y,
                ),
            );
            painter
                .with_clip_rect(text_region)
                .galley(text_pos, galley, text_color);

            let icon_size: f32 = 14.0;
            let chevron_rect = egui::Rect::from_center_size(
                egui::pos2(rect.max.x - chevron_width / 2.0 - 2.0, rect.center().y),
                egui::vec2(icon_size, icon_size),
            );
            crate::icons::paint_icon::paint_icon(
                painter,
                chevron_rect,
                &crate::icons::lucide_icon::LucideIcon::ChevronDown,
                theme.muted_foreground,
            );

            if response.has_focus() {
                crate::paint::paint_focus_ring::paint_focus_ring(
                    painter,
                    rect,
                    style.corner_radius,
                    theme.ring,
                );
            }
        }

        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        let toggle_cmd = if response.clicked() {
            Some(egui::SetOpenCommand::Toggle)
        } else {
            None
        };

        let popup_cr = crate::utils::f32_to_u8_clamped(style.corner_radius);
        let popup_width = super::select_sizing::popup_width(
            ui,
            width,
            entries.iter().map(|(_, label)| label.as_str()),
            200.0,
            144.0,
        );
        let popup = egui::Popup::new(popup_id, ui.ctx().clone(), &response, ui.layer_id())
            .open_memory(toggle_cmd)
            .frame(
                egui::Frame::NONE
                    .fill(style.popover_bg)
                    .inner_margin(egui::Margin::same(4))
                    .corner_radius(egui::CornerRadius::same(popup_cr))
                    .stroke(egui::Stroke::new(1.0, style.popover_border))
                    .shadow(egui::Shadow {
                        offset: [0, 4],
                        blur: 12,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(8),
                    }),
            );

        let _ = popup.show(|popup_ui: &mut egui::Ui| {
            popup_ui.set_min_width(popup_width);
            popup_ui.set_max_width(popup_width);
            let check_icon_size: f32 = 12.0;

            for (value, label) in entries {
                let is_selected = selected.as_ref().is_some_and(|current| current == value);

                let galley = popup_ui.painter().layout_no_wrap(
                    label.clone(),
                    egui::FontId::proportional(14.0),
                    style.item_text,
                );

                let item_height = galley.size().y + 8.0;
                let item_desired = egui::vec2(popup_width, item_height);
                let (item_rect, item_response) =
                    popup_ui.allocate_exact_size(item_desired, egui::Sense::click());

                if popup_ui.is_rect_visible(item_rect) {
                    let item_cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                        (style.corner_radius - 2.0).max(4.0),
                    ));
                    if item_response.hovered() {
                        let _ =
                            popup_ui
                                .painter()
                                .rect_filled(item_rect, item_cr, style.item_hover_bg);
                        popup_ui
                            .ctx()
                            .set_cursor_icon(egui::CursorIcon::PointingHand);
                    }

                    let text_x = item_rect.min.x + 6.0;
                    if is_selected {
                        let check_rect = egui::Rect::from_min_size(
                            egui::pos2(
                                item_rect.max.x - check_icon_size - 8.0,
                                item_rect.center().y - check_icon_size / 2.0,
                            ),
                            egui::vec2(check_icon_size, check_icon_size),
                        );
                        crate::icons::paint_icon::paint_icon(
                            popup_ui.painter(),
                            check_rect,
                            &crate::icons::lucide_icon::LucideIcon::Check,
                            style.item_text,
                        );
                    }

                    let text_region = egui::Rect::from_min_max(
                        egui::pos2(text_x, item_rect.min.y),
                        egui::pos2(
                            (item_rect.max.x - check_icon_size - 12.0).max(text_x),
                            item_rect.max.y,
                        ),
                    );
                    popup_ui.painter().with_clip_rect(text_region).galley(
                        egui::pos2(text_x, item_rect.center().y - galley.size().y / 2.0),
                        galley,
                        style.item_text,
                    );
                }

                if item_response.clicked() {
                    *selected = Some(value.clone());
                    egui::Popup::close_id(popup_ui.ctx(), popup_id);
                    popup_ui.ctx().request_repaint();
                }
            }
        });

        response
    }
}

// ---------------------------------------------------------------------------
// SelectValue — non-Option variant
// ---------------------------------------------------------------------------

impl<T: Clone + std::fmt::Display + PartialEq + 'static> egui::Widget
    for super::widget::SelectValue<'_, T>
{
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let entries: Vec<(T, String)> = self
            .options
            .iter()
            .map(|option| (option.clone(), option.to_string()))
            .collect();
        super::widget::SelectValueLabeled::show_labeled(
            ui,
            self.selected,
            &entries,
            self.width,
            self.selected_text_override.as_deref(),
        )
    }
}

impl<T: Clone + PartialEq + 'static> egui::Widget for super::widget::SelectValueLabeled<'_, T> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        Self::show_labeled(ui, self.selected, self.entries, self.width, None)
    }
}

impl<T: Clone + PartialEq + 'static> super::widget::SelectValueLabeled<'_, T> {
    fn show_labeled(
        ui: &mut egui::Ui,
        selected: &mut T,
        entries: &[(T, String)],
        width_opt: Option<f32>,
        text_override: Option<&str>,
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let style = super::select_style::resolve_select_style(&theme);
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let height = spacing.touch_height;
        let h_padding: f32 = spacing.touch_padding;
        let chevron_width: f32 = 20.0;
        let width = super::select_sizing::trigger_width(
            ui,
            width_opt,
            entries
                .iter()
                .map(|(_, label)| label.as_str())
                .chain(text_override),
            h_padding,
            chevron_width,
            200.0,
        );
        let desired = egui::vec2(width, height);

        let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::click());
        let popup_id = response.id.with("popup");

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(style.corner_radius));
            let pressed = response.is_pointer_button_down_on();
            let trigger_bg = if pressed {
                crate::paint::interpolate_color::interpolate_color(
                    style.trigger_bg,
                    theme.accent,
                    0.85,
                )
            } else if response.hovered() {
                theme.accent
            } else {
                style.trigger_bg
            };
            let trigger_border = if response.hovered() || pressed {
                theme.ring
            } else {
                style.trigger_border
            };

            let _ = painter.rect_filled(rect, cr, trigger_bg);
            let _ = painter.rect_stroke(
                rect,
                cr,
                egui::Stroke::new(1.0, trigger_border),
                egui::epaint::StrokeKind::Inside,
            );

            let display_text = if let Some(override_text) = text_override {
                override_text.to_owned()
            } else {
                entries
                    .iter()
                    .find(|entry| entry.0 == *selected)
                    .map_or_else(String::new, |(_, label)| label.clone())
            };

            let galley = painter.layout_no_wrap(
                display_text,
                egui::FontId::proportional(14.0),
                style.trigger_text,
            );
            let text_pos = egui::pos2(
                rect.min.x + h_padding,
                rect.center().y - galley.size().y / 2.0,
            );
            let text_region = egui::Rect::from_min_max(
                egui::pos2(rect.min.x + h_padding, rect.min.y),
                egui::pos2(
                    (rect.max.x - h_padding - chevron_width).max(rect.min.x),
                    rect.max.y,
                ),
            );
            painter
                .with_clip_rect(text_region)
                .galley(text_pos, galley, style.trigger_text);

            let icon_size: f32 = 14.0;
            let chevron_rect = egui::Rect::from_center_size(
                egui::pos2(rect.max.x - chevron_width / 2.0 - 2.0, rect.center().y),
                egui::vec2(icon_size, icon_size),
            );
            crate::icons::paint_icon::paint_icon(
                painter,
                chevron_rect,
                &crate::icons::lucide_icon::LucideIcon::ChevronDown,
                theme.muted_foreground,
            );

            if response.has_focus() {
                crate::paint::paint_focus_ring::paint_focus_ring(
                    painter,
                    rect,
                    style.corner_radius,
                    theme.ring,
                );
            }
        }

        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        let toggle_cmd = if response.clicked() {
            Some(egui::SetOpenCommand::Toggle)
        } else {
            None
        };

        let popup_cr = crate::utils::f32_to_u8_clamped(style.corner_radius);
        let popup_width = super::select_sizing::popup_width(
            ui,
            width,
            entries.iter().map(|(_, label)| label.as_str()),
            200.0,
            144.0,
        );
        let popup = egui::Popup::new(popup_id, ui.ctx().clone(), &response, ui.layer_id())
            .open_memory(toggle_cmd)
            .frame(
                egui::Frame::NONE
                    .fill(style.popover_bg)
                    .inner_margin(egui::Margin::same(4))
                    .corner_radius(egui::CornerRadius::same(popup_cr))
                    .stroke(egui::Stroke::new(1.0, style.popover_border))
                    .shadow(egui::Shadow {
                        offset: [0, 4],
                        blur: 12,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(8),
                    }),
            );

        let _ = popup.show(|popup_ui: &mut egui::Ui| {
            popup_ui.set_min_width(popup_width);
            popup_ui.set_max_width(popup_width);
            let check_icon_size: f32 = 12.0;

            for (value, label) in entries {
                let is_selected = *selected == *value;

                let galley = popup_ui.painter().layout_no_wrap(
                    label.clone(),
                    egui::FontId::proportional(14.0),
                    style.item_text,
                );

                let item_height = galley.size().y + 8.0;
                let item_desired = egui::vec2(popup_width, item_height);
                let (item_rect, item_response) =
                    popup_ui.allocate_exact_size(item_desired, egui::Sense::click());

                if popup_ui.is_rect_visible(item_rect) {
                    let item_cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                        (style.corner_radius - 2.0).max(4.0),
                    ));
                    if item_response.hovered() {
                        let _ =
                            popup_ui
                                .painter()
                                .rect_filled(item_rect, item_cr, style.item_hover_bg);
                        popup_ui
                            .ctx()
                            .set_cursor_icon(egui::CursorIcon::PointingHand);
                    }

                    let text_x = item_rect.min.x + 6.0;
                    if is_selected {
                        let check_rect = egui::Rect::from_min_size(
                            egui::pos2(
                                item_rect.max.x - check_icon_size - 8.0,
                                item_rect.center().y - check_icon_size / 2.0,
                            ),
                            egui::vec2(check_icon_size, check_icon_size),
                        );
                        crate::icons::paint_icon::paint_icon(
                            popup_ui.painter(),
                            check_rect,
                            &crate::icons::lucide_icon::LucideIcon::Check,
                            style.item_text,
                        );
                    }

                    let text_region = egui::Rect::from_min_max(
                        egui::pos2(text_x, item_rect.min.y),
                        egui::pos2(
                            (item_rect.max.x - check_icon_size - 12.0).max(text_x),
                            item_rect.max.y,
                        ),
                    );
                    popup_ui.painter().with_clip_rect(text_region).galley(
                        egui::pos2(text_x, item_rect.center().y - galley.size().y / 2.0),
                        galley,
                        style.item_text,
                    );
                }

                if item_response.clicked() {
                    *selected = value.clone();
                    egui::Popup::close_id(popup_ui.ctx(), popup_id);
                    popup_ui.ctx().request_repaint();
                }
            }
        });

        response
    }
}
