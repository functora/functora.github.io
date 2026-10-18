use crate::app::CryptonoteApp;
use crate::storage::APP_ATTRS;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Card, TextareaPasteClear};

const BLOCK_GAP: f32 = 20.0;

impl CryptonoteApp {
    pub(crate) fn screen_donate(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let intro = BaseMsg::DonateIntro.render(lang);
        let parts = intro.split("Functora").collect::<Vec<_>>();
        let last = parts.len().saturating_sub(1);
        _ = Card::new().show(ui, |inner| {
            inner.expand_to_include_x(inner.max_rect().right());
            _ = parts
                .into_iter()
                .enumerate()
                .fold(
                    functora_egui::Hypertext::new().text(format!("{} ", BaseMsg::DonateGreeting.render(lang))),
                    |acc, (idx, part)| {
                        if idx == last {
                            acc.text(part)
                        } else {
                            acc.text(part).link("Functora", APP_ATTRS.author_url())
                        }
                    },
                )
                .size(13.0)
                .show(inner);
        });
        let () = ui.add_space(8.0);
        for block in functora_egui::white_label::donate_blocks() {
            _ = ui.label(egui::RichText::new(&block.label).size(16.0).weak());
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
