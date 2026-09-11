use crate::app::CryptonoteApp;
use crate::route::Screen;
use egui::ScrollArea;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant};

impl CryptonoteApp {
    pub(crate) fn screen_license(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = ui.label(
            egui::RichText::new(BaseMsg::TermsOfServiceTitle.render(lang))
                .size(20.0)
                .strong(),
        );
        let () = ui.add_space(8.0);
        _ = ScrollArea::vertical().show(ui, |inner| {
            _ = inner.label(BaseMsg::LicenseText.render(lang));
        });
        let () = ui.add_space(8.0);
        if ui
            .add(
                Button::new(BaseMsg::Back.render(lang))
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .clicked()
        {
            self.navigate(Screen::Home);
        }
    }
}
