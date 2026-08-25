use crate::app::{APP_ATTRS, CryptonoteApp};
use crate::messages::Msg;
use crate::screens::Screen;
use functora_core::messages::Msg as BaseMsg;
use functora_core::white_label::donate_blocks;
use functora_egui::{Button, ButtonVariant, Card, ComponentSize};

impl CryptonoteApp {
    pub(crate) fn render_about(&mut self, ui: &mut egui::Ui) {
        let heading = format!("{} v{}", APP_ATTRS.app_name(), APP_ATTRS.vsn);
        let text = self.text(&Msg::AboutText);
        let _ = Card::new().heading(heading).show(ui, |card| {
            let _scroll = egui::ScrollArea::vertical().show(card, |scroll| {
                _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
            });
        });
        let donate_label = self.text(&Msg::Base(BaseMsg::Donate));
        let terms_label = self.text(&Msg::Base(BaseMsg::TermsOfServiceTitle));
        let privacy_label = self.text(&Msg::Base(BaseMsg::PrivacyPolicyTitle));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row.add(Button::new(&donate_label)).clicked() {
                app.navigate(Screen::Donate);
            }
            if row
                .add(Button::new(&terms_label).variant(ButtonVariant::Outline))
                .clicked()
            {
                app.navigate(Screen::License);
            }
            if row
                .add(Button::new(&privacy_label).variant(ButtonVariant::Outline))
                .clicked()
            {
                app.navigate(Screen::Privacy);
            }
        });
    }

    pub(crate) fn render_donate(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::Base(BaseMsg::Donate));
        let greeting = self.text(&Msg::Base(BaseMsg::DonateGreeting));
        let intro = self.text(&Msg::Base(BaseMsg::DonateIntro));
        let copy_label = self.text(&Msg::Base(BaseMsg::Copy));
        let _ = Card::new().heading(heading).show(ui, |card| {
            _ = card.label(greeting);
            _ = card.label(intro);
            for (i, block) in donate_blocks().into_iter().enumerate() {
                if i > 0 {
                    _ = card.separator();
                }
                let _row = card.horizontal_wrapped(|row| {
                    _ = row.label(egui::RichText::new(block.label).strong());
                    if row
                        .add(
                            Button::new(&copy_label)
                                .variant(ButtonVariant::Outline)
                                .size(ComponentSize::Sm),
                        )
                        .on_hover_text(&copy_label)
                        .clicked()
                    {
                        self.copy_text(block.address.clone());
                    }
                });
                _ = card.add(
                    egui::Label::new(egui::RichText::new(&block.address).monospace())
                        .wrap()
                        .selectable(true),
                );
            }
        });
    }

    pub(crate) fn render_license(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::Base(BaseMsg::TermsOfServiceTitle));
        let text = self.text(&Msg::Base(BaseMsg::LicenseText));
        let _ = Card::new().heading(heading).show(ui, |card| {
            let _scroll = egui::ScrollArea::vertical().show(card, |scroll| {
                _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
            });
        });
    }

    pub(crate) fn render_privacy(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::Base(BaseMsg::PrivacyPolicyTitle));
        let text = self.text(&Msg::Base(BaseMsg::PrivacyText));
        let _ = Card::new().heading(heading).show(ui, |card| {
            let _scroll = egui::ScrollArea::vertical().show(card, |scroll| {
                _ = scroll.add(egui::Label::new(text).wrap().selectable(true));
            });
        });
    }
}
