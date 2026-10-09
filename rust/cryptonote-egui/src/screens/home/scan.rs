use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::hooks::handle_open_url;
use crate::messages::Msg;
use functora_egui::i18n::I18N;
use functora_egui::{Progress, ToastVariant};

impl CryptonoteApp {
    pub(crate) fn home_scan(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let () = ui.add_space(8.0);
        _ = functora_egui::QrScanner::new()
            .continuous(true)
            .show(ui, &mut self.qr_state, lang);
        if let Some(text) = self.qr_state.decoded() {
            self.qr_state.clear_decoded();
            match handle_open_url(&text, &mut self.temporary) {
                Ok(screen) => self.navigate(screen),
                Err(e) => self.toast.add(
                    Msg::Error(crate::error::MsgError::from(e)).render(lang),
                    ToastVariant::Error,
                    toast_time,
                ),
            }
        }
        if let Some(err) = self.qr_state.error() {
            let text = Msg::Error(crate::error::MsgError::from(AppError::QrScan(err))).render(lang);
            if self.unseen_qr_error(&text) {
                self.toast.add(text, ToastVariant::Error, toast_time);
            }
        } else {
            self.qr_error_notified = None;
        }
        if let Some(job) = self.temporary.progress.clone() {
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
        }
    }
}
