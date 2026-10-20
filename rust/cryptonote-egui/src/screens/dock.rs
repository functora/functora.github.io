use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, FlexInst};

use crate::app::CryptonoteApp;

impl CryptonoteApp {
    pub(crate) fn dock_back(&mut self, ui: &mut egui::Ui) {
        if !self.router.history().can_go_back() {
            return;
        }
        let lang = self.lang();
        let () = ui.add_space(8.0);
        if ui.add(Self::back_button(lang)).clicked() {
            self.go_back();
        }
    }

    pub(crate) fn dock_back_flex(&mut self, f: &mut FlexInst) {
        if !self.router.history().can_go_back() {
            return;
        }
        if f.add(Self::back_button(self.lang())).inner.clicked() {
            self.go_back();
        }
    }

    fn back_button(lang: functora_egui::i18n::Language) -> Button<'static> {
        Button::new(BaseMsg::Back.render(lang))
            .icon(functora_egui::LucideIcon::ArrowLeft)
            .variant(ButtonVariant::Outline)
    }
}
