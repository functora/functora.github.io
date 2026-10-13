use crate::app::CryptonoteApp;
use functora_egui::TextareaPasteClear;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

const BLOCK_GAP: f32 = 20.0;

impl CryptonoteApp {
    pub(crate) fn screen_donate(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        _ = ui.label(egui::RichText::new(BaseMsg::Donate.render(lang)).size(20.0).strong());
        let () = ui.add_space(8.0);
        _ = ui.label(BaseMsg::DonateGreeting.render(lang));
        let () = ui.add_space(4.0);
        _ = ui.label(BaseMsg::DonateIntro.render(lang));
        let () = ui.add_space(8.0);
        for block in functora_egui::white_label::donate_blocks() {
            _ = ui.label(egui::RichText::new(&block.label).strong());
            let () = ui.add_space(4.0);
            _ = functora_egui::QrImage::new(&block.address).show(ui);
            let () = ui.add_space(4.0);
            let mut address = block.address.clone();
            let resp = TextareaPasteClear::new(&mut address)
                .readonly()
                .min_height(80.0)
                .show(ui);
            self.paste_clear_feedback(resp, toast_time);
            let () = ui.add_space(BLOCK_GAP);
        }
        self.dock_back(ui);
    }
}
