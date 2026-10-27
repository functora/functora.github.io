//! Show method for `PropertyRow`.

impl super::property_row::PropertyRow {
    /// Renders a property label and a control area.
    pub fn show(self, ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let key = super::property_grid_context_key::property_grid_context_key();
        let context = ui
            .ctx()
            .data(|data| data.get_temp::<super::property_grid_context::PropertyGridContext>(key));
        let label_width = self
            .label_width
            .or_else(|| context.map(|ctx| ctx.label_width))
            .unwrap_or(112.0);

        ui.horizontal(|inner_ui| {
            let column_width = label_width.min(inner_ui.available_width());
            let label_rect = inner_ui
                .allocate_exact_size(
                    egui::vec2(column_width, inner_ui.spacing().interact_size.y),
                    egui::Sense::hover(),
                )
                .0;
            let galley = crate::text::layout(
                inner_ui,
                self.label,
                egui::FontId::proportional(12.0),
                theme.muted_foreground,
                label_rect.width(),
                Some(crate::text::Wrap::Truncate),
            );
            inner_ui.painter().galley(
                egui::pos2(
                    label_rect.min.x,
                    label_rect.center().y - galley.size().y / 2.0,
                ),
                galley,
                theme.muted_foreground,
            );
            content(inner_ui);
        })
        .response
    }
}
