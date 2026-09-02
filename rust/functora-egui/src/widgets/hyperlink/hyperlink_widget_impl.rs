//! Widget trait implementation for Hyperlink.

impl egui::Widget for super::widget::Hyperlink {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let previous = ui.visuals().hyperlink_color;
        ui.visuals_mut().hyperlink_color = theme.primary;
        let response = if let Some(url) = self.url {
            ui.add(
                egui::Hyperlink::from_label_and_url(self.label, url).open_in_new_tab(self.new_tab),
            )
        } else {
            ui.add(egui::Link::new(self.label))
        };
        ui.visuals_mut().hyperlink_color = previous;
        response
    }
}
