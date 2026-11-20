use crate::state::ShowcaseApp;
use functora_egui::i18n::Language;
use functora_egui::white_label::{WhiteLabelContent, screen_donate};

impl ShowcaseApp {
    pub(crate) fn demo_donate(&mut self, ui: &mut egui::Ui, lang: Language) {
        let now = ui.ctx().input(|i| i.time);
        let content = WhiteLabelContent::<functora_egui::messages::Msg> {
            donate_greeting: Some(functora_egui::messages::Msg::DonateGreeting),
            donate_intro: Some(functora_egui::messages::Msg::DonateIntro),
            ..Default::default()
        };
        screen_donate(ui, &Self::DEMO_ATTRS, lang, &content, now, &mut self.toast);
    }
}
