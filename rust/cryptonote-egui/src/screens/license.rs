use crate::app::CryptonoteApp;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn screen_license(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        // Whole-screen document text flows in the page scroll: no nested
        // scroll container, so there is exactly one scrollbar and no scroll
        // offset shared with other screens.
        _ = ui.label(BaseMsg::LicenseText.render(lang));
        self.dock_back(ui);
    }
}
