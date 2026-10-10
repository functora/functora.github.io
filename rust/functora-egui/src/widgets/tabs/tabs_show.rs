//! Show method for Tabs — renders tab bar and content.

impl super::widget::Tabs {
    /// Shows the tab bar and content. `selected` is the currently active tab index.
    /// Calls `content(ui, selected_index)` for the active tab's body.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        selected: &mut usize,
        content: impl FnOnce(&mut egui::Ui, usize),
    ) -> egui::Response {
        let entries: Vec<(usize, String)> = self.labels.into_iter().enumerate().collect();
        let mut typed = super::widget::TabsValue::new(&entries);
        if self.fill_width {
            typed = typed.fill_width();
        }
        typed.show(ui, selected, |tab_ui, value| {
            content(tab_ui, *value);
        })
    }
}

// ---------------------------------------------------------------------------
// TabsValue — enum-bound tabs
// ---------------------------------------------------------------------------

impl<T: Clone + PartialEq> super::widget::TabsValue<'_, T> {
    /// Shows the tab bar and content. `selected` holds the active value.
    /// Calls `content(ui, selected_value)` for the active tab's body.
    /// A value missing from `entries` keeps the current selection.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        selected: &mut T,
        content: impl FnOnce(&mut egui::Ui, &T),
    ) -> egui::Response {
        let Self {
            entries,
            fill_width,
        } = self;
        let count = entries.len();
        let fill = fill_width && count > 0;

        ui.vertical(|inner_ui| {
            let avail = inner_ui.available_width().max(0.0);
            let share = fill.then(|| avail / crate::utils::usize_to_f32(count).max(1.0));
            let _ = crate::widgets::button_group::widget::ButtonGroup::show(inner_ui, |group_ui| {
                for (value, label) in entries {
                    let is_active = *value == *selected;
                    let mut button = crate::widgets::button::widget::Button::new(label.clone())
                        .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                        .selected(is_active);
                    if let Some(width) = share {
                        button = button.fixed_width(width);
                    }
                    if button.show(group_ui).clicked() {
                        *selected = value.clone();
                        group_ui.ctx().request_repaint();
                    }
                }
            });

            inner_ui.add_space(8.0);

            content(inner_ui, selected);
        })
        .response
    }
}

// ---------------------------------------------------------------------------
// IconTabs — icon+tooltip variant
// ---------------------------------------------------------------------------

impl super::widget::IconTabs {
    pub fn show(
        self,
        ui: &mut egui::Ui,
        selected: &mut usize,
        content: impl FnOnce(&mut egui::Ui, usize),
    ) -> egui::Response {
        let fill = self.fill_width;
        let entries: Vec<(usize, super::widget::TabEntry)> =
            self.entries.into_iter().enumerate().collect();
        let mut typed = super::widget::IconTabsValue::new(&entries);
        if fill {
            typed = typed.fill_width();
        }
        typed.show(ui, selected, |tab_ui, value| {
            content(tab_ui, *value);
        })
    }
}

// ---------------------------------------------------------------------------
// IconTabsValue — enum-bound icon tabs
// ---------------------------------------------------------------------------

impl<T: Clone + PartialEq> super::widget::IconTabsValue<'_, T> {
    /// Shows the icon tab bar and content. `selected` holds the active value.
    /// Calls `content(ui, selected_value)` for the active tab's body.
    /// A value missing from `entries` keeps the current selection.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        selected: &mut T,
        content: impl FnOnce(&mut egui::Ui, &T),
    ) -> egui::Response {
        let Self {
            entries,
            fill_width,
        } = self;
        let count = entries.len();
        let fill = fill_width && count > 0;

        ui.vertical(|inner_ui| {
            let avail = inner_ui.available_width().max(0.0);
            let share = fill.then(|| avail / crate::utils::usize_to_f32(count).max(1.0));
            let _ = crate::widgets::button_group::widget::ButtonGroup::show(inner_ui, |group_ui| {
                for (value, entry) in entries {
                    let is_active = *value == *selected;
                    let response = match entry {
                        super::widget::TabEntry::Text(label) => {
                            let mut button =
                                crate::widgets::button::widget::Button::new(label.clone())
                                    .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                                    .selected(is_active);
                            if let Some(width) = share {
                                button = button.fixed_width(width);
                            }
                            button.show(group_ui)
                        }
                        super::widget::TabEntry::Icon { icon, tooltip } => {
                            let mut button =
                                crate::widgets::button::widget::Button::icon_only(*icon)
                                    .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                                    .selected(is_active);
                            if let Some(width) = share {
                                button = button.fixed_width(width);
                            }
                            let response = button.show(group_ui);
                            let _ = response.clone().on_hover_text(tooltip);
                            response
                        }
                    };
                    if response.clicked() {
                        *selected = value.clone();
                        group_ui.ctx().request_repaint();
                    }
                }
            });

            inner_ui.add_space(8.0);
            content(inner_ui, selected);
        })
        .response
    }
}
