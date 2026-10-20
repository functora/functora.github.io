mod attachments;
mod create;
mod open;
mod scan;

use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::state::ActionMode;
use functora_egui::i18n::I18N;
use functora_egui::{Label, TabsValue};

impl CryptonoteApp {
    pub(crate) fn screen_home(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let () = ui.add_space(4.0);
        _ = Label::new(Msg::ActionLabel.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        let mut tab = self.temporary.action;
        let entries = [
            (ActionMode::Create, Msg::ActionCreate.render(lang)),
            (ActionMode::Open, Msg::ActionOpen.render(lang)),
            (ActionMode::Scan, Msg::ActionScan.render(lang)),
        ];
        _ = TabsValue::new(&entries)
            .fill_width()
            .show(ui, &mut tab, |tab_ui, mode| {
                self.temporary.action = *mode;
                match self.temporary.action {
                    ActionMode::Create => self.home_create(tab_ui),
                    ActionMode::Open => self.home_open(tab_ui),
                    ActionMode::Scan => self.home_scan(tab_ui),
                }
            });
        let () = ui.add_space(12.0);
    }
}
