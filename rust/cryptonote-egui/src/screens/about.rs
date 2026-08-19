use crate::app::{CryptonoteApp, APP_ATTRS};
use crate::messages::Msg;
use crate::screens::Screen;
use functora_core::messages::Msg as BaseMsg;
use functora_core::white_label::donate_blocks;

impl CryptonoteApp {
    pub(crate) fn render_about(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(format!("{} v{}", APP_ATTRS.app_name(), APP_ATTRS.vsn));
        let text = self.text(&Msg::AboutText);
        let _scroll = egui::ScrollArea::vertical().show(ui, |scroll| {
            _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
        });
        _ = ui.separator();
        let _links = ui.horizontal_wrapped(|row| {
            if row.button(self.text(&Msg::Base(BaseMsg::Donate))).clicked() {
                self.navigate(Screen::Donate);
            }
            if row
                .button(self.text(&Msg::Base(BaseMsg::TermsOfServiceTitle)))
                .clicked()
            {
                self.navigate(Screen::License);
            }
            if row
                .button(self.text(&Msg::Base(BaseMsg::PrivacyPolicyTitle)))
                .clicked()
            {
                self.navigate(Screen::Privacy);
            }
        });
    }

    pub(crate) fn render_donate(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::Base(BaseMsg::Donate)));
        _ = ui.label(self.text(&Msg::Base(BaseMsg::DonateGreeting)));
        _ = ui.label(self.text(&Msg::Base(BaseMsg::DonateIntro)));
        for block in donate_blocks() {
            _ = ui.separator();
            _ = ui.label(block.label);
            let address = block.address;
            _ = ui
                .add(egui::Label::new(egui::RichText::new(&address).monospace()).selectable(true));
            if ui.button(self.text(&Msg::Base(BaseMsg::Copy))).clicked() {
                self.copy_text(address);
            }
        }
    }

    pub(crate) fn render_license(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::Base(BaseMsg::TermsOfServiceTitle)));
        let text = self.text(&Msg::Base(BaseMsg::LicenseText));
        let _scroll = egui::ScrollArea::vertical().show(ui, |scroll| {
            _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
        });
    }

    pub(crate) fn render_privacy(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::Base(BaseMsg::PrivacyPolicyTitle)));
        let text = self.text(&Msg::Base(BaseMsg::PrivacyText));
        let _scroll = egui::ScrollArea::vertical().show(ui, |scroll| {
            _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
        });
    }
}
