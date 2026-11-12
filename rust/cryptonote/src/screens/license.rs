use crate::app::CryptonoteApp;
use functora_egui::white_label::{screen_license, screen_privacy};

impl CryptonoteApp {
    pub(crate) fn screen_license(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        screen_license(ui, &mut self.md_cache, &crate::storage::APP_ATTRS, lang);
        self.dock_back(ui);
    }

    pub(crate) fn screen_privacy(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        screen_privacy(ui, &mut self.md_cache, &crate::storage::APP_ATTRS, lang);
        self.dock_back(ui);
    }
}
