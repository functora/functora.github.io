mod attachments;
mod create;
mod open;
mod scan;

use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::state::ActionMode;
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Label, Progress, TabsValue};

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
        if let Some(job) = self.temporary.progress.clone() {
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            let () = ui.add_space(4.0);
            _ = Label::new(format!(
                "{} {} / {}",
                BaseMsg::Stage(job.stage).render(lang),
                job.done,
                job.total
            ))
            .show(ui);
        }
    }

    pub(crate) fn show_pick_overlay(&mut self, ctx: &egui::Context, lang: Language) {
        if self.pick_rx.is_none() {
            return;
        }
        if let Some(cancel) = self.pick_cancel.clone() {
            let mut open = self.pick_overlay_open;
            functora_egui::BlockingOverlay::new(BaseMsg::PickingFiles.render(lang)).show(
                ctx,
                &mut open,
                self.temporary.progress.as_ref(),
                &cancel,
                lang,
            );
            self.pick_overlay_open = open;
        }
    }
}
