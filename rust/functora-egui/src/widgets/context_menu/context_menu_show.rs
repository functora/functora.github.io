//! Show method for `ContextMenu` — renders a right-click popup.

impl super::widget::ContextMenu {
    /// Attaches a context menu to `response`. Shows on right-click.
    /// Calls `on_select(index)` when an item is clicked.
    pub fn show(response: &egui::Response, items: &[&str], on_select: impl FnOnce(usize)) {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(&response.ctx);
        let mut selected_idx = None;

        let _ = response.context_menu(|inner_ui| {
            // Compute max text width for a tight menu
            let max_text_width: f32 = items
                .iter()
                .map(|label| {
                    inner_ui
                        .painter()
                        .layout_no_wrap(
                            label.to_string(),
                            egui::FontId::proportional(14.0),
                            theme.popover_foreground,
                        )
                        .size()
                        .x
                })
                .fold(0.0_f32, f32::max);

            let mut menu_width = (max_text_width + 24.0).max(120.0);
            let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(
                inner_ui.ctx(),
            );
            let screen_w = inner_ui.ctx().input(|i| i.viewport_rect().width());
            menu_width = menu_width
                .min(screen_w - 2.0 * spacing.page_padding - 16.0)
                .max(120.0);
            if spacing.is_mobile() {
                menu_width = menu_width
                    .max(200.0)
                    .min(screen_w - 2.0 * spacing.page_padding);
            }
            inner_ui.set_min_width(menu_width);
            inner_ui.set_max_width(menu_width);

            for (idx, &label) in items.iter().enumerate() {
                let galley = inner_ui.painter().layout_no_wrap(
                    label.to_owned(),
                    egui::FontId::proportional(14.0),
                    theme.popover_foreground,
                );
                let desired = egui::vec2(menu_width, galley.size().y + 8.0);
                let (rect, r) = inner_ui.allocate_exact_size(desired, egui::Sense::click());

                if r.hovered() {
                    let _ = inner_ui.painter().rect_filled(
                        rect,
                        egui::CornerRadius::same(4),
                        theme.accent,
                    );
                }

                if inner_ui.is_rect_visible(rect) {
                    inner_ui.painter().galley(
                        egui::pos2(rect.min.x + 8.0, rect.center().y - galley.size().y / 2.0),
                        galley,
                        theme.popover_foreground,
                    );
                }

                if r.clicked() {
                    selected_idx = Some(idx);
                    inner_ui.close();
                }
            }
        });

        if let Some(idx) = selected_idx {
            on_select(idx);
        }
    }
}
