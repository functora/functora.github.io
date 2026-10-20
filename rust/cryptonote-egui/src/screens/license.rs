use crate::app::CryptonoteApp;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn screen_license(&mut self, ui: &mut egui::Ui) {
        self.screen_markdown_doc(ui, &BaseMsg::LicenseText.render(self.lang()));
    }

    pub(crate) fn screen_privacy(&mut self, ui: &mut egui::Ui) {
        self.screen_markdown_doc(ui, &BaseMsg::PrivacyText.render(self.lang()));
    }

    fn screen_markdown_doc(&mut self, ui: &mut egui::Ui, text: &str) {
        // Whole-screen document text flows in the page scroll: no nested
        // scroll container, so there is exactly one scrollbar and no scroll
        // offset shared with other screens.
        _ = functora_egui::markdown_view::show(ui, &mut self.md_cache, text);
        self.dock_back(ui);
    }
}
