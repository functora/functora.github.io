//! Footer builder — centered, muted, flex-wrapped footer similar to `pub/functora-css`.

#[must_use]
pub struct Footer {}

impl Default for Footer {
    fn default() -> Self {
        Self::new()
    }
}

impl Footer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn show(self, ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let frame = egui::Frame::NONE
            .fill(theme.background)
            .inner_margin(egui::Margin::symmetric(8, 4));
        frame
            .show(ui, |inner| {
                let _ =
                    inner.with_layout(egui::Layout::top_down(egui::Align::Center), |centered| {
                        content(centered);
                    });
            })
            .response
    }
}
