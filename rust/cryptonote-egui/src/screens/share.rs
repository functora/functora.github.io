use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use crate::route::Screen;
use crate::state::ActionMode;
use crate::state::External;
use functora_egui::i18n::I18N;
use functora_egui::{Button, ButtonVariant, Flex, Progress, TextareaPasteClear};

impl CryptonoteApp {
    pub(crate) fn screen_share(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let url = self.temporary.external.clone().note_url();
        let pkg_ready = matches!(self.temporary.external, External::Archive(_));
        if pkg_ready {
            _ = ui.label(egui::RichText::new(Msg::ArchiveReady.render(lang)).size(16.0).strong());
            let () = ui.add_space(12.0);
        } else if !url.is_empty() {
            _ = functora_egui::QrImage::new(&url).show(ui);
            let () = ui.add_space(8.0);
            let mut url_text = url.clone();
            let resp = TextareaPasteClear::new(&mut url_text)
                .readonly()
                .min_height(60.0)
                .show(ui);
            self.paste_clear_feedback(resp, toast_time);
            let () = ui.add_space(8.0);
        } else {
            _ = ui.label(Msg::Base(functora_egui::messages::Msg::Loading).render(lang));
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(0.5));
        }
        let () = ui.add_space(12.0);
        if pkg_ready || !url.is_empty() {
            _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
                if !url.is_empty()
                    && f.add(
                        Button::new(Msg::Share.render(lang))
                            .icon(functora_egui::LucideIcon::Share2)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                {
                    let data = functora_egui::share::ShareData {
                        title: "Cryptonote".to_string(),
                        text: Msg::SharedNoteText.render(lang),
                        url: url.clone(),
                    };
                    let rx = functora_egui::spawn_async(async move {
                        functora_egui::share::share(data).await.map_err(AppError::from)
                    });
                    self.share_rx = Some(rx);
                }
                if pkg_ready
                    && f.add(
                        Button::new(Msg::Download.render(lang))
                            .icon(functora_egui::LucideIcon::Download)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                {
                    let bytes = self.temporary.external.clone().archive_bytes();
                    if !bytes.is_empty() && claim_job(&mut self.temporary.progress, Stage::Download).is_some() {
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::download::download(bytes, "archive.cryptonote")
                                .await
                                .map_err(AppError::from)
                        });
                        self.download_rx = Some(rx);
                    }
                }
                if f.add(
                    Button::new(Msg::ViewButton.render(lang))
                        .icon(functora_egui::LucideIcon::Eye)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    self.navigate(Screen::View);
                }
                if f.add(
                    Button::new(Msg::EditNote.render(lang))
                        .icon(functora_egui::LucideIcon::SquarePen)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    self.temporary.action = ActionMode::Create;
                    self.navigate(Screen::Home);
                }
                if f.add(
                    Button::new(Msg::CreateNewNote.render(lang))
                        .icon(functora_egui::LucideIcon::Trash2)
                        .variant(ButtonVariant::Ghost),
                )
                .inner
                .clicked()
                {
                    self.reset();
                }
            });
            if let Some(job) = self.temporary.progress.clone() {
                let () = ui.add_space(8.0);
                _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            }
        }
    }
}
