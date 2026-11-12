use crate::app::CryptonoteApp;
use crate::storage::APP_ATTRS;
use functora_egui::white_label::{WhiteLabelContent, screen_donate};

impl CryptonoteApp {
    pub(crate) fn screen_donate(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let content = WhiteLabelContent::<functora_egui::messages::Msg> {
            donate_greeting: Some(functora_egui::messages::Msg::DonateGreeting),
            donate_intro: Some(functora_egui::messages::Msg::DonateIntro),
            ..Default::default()
        };
        screen_donate(ui, &APP_ATTRS, lang, &content, toast_time, &mut self.toast);
        self.dock_back(ui);
    }
}
