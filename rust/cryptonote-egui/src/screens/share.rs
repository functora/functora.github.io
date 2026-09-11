use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use crate::route::Screen;
use crate::state::ActionMode;
use crate::state::External;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Alert, Button, ButtonVariant, Flex, Progress, Textarea};

impl CryptonoteApp {
    pub(crate) fn screen_share(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let (url, qr) = match &self.temporary.external {
            External::Note(n) => (n.url.clone(), n.qr.clone()),
            _ => (String::new(), String::new()),
        };
        let pkg_ready = matches!(self.temporary.external, External::Archive(_));
        if let Some(msg) = self.temporary.message.clone() {
            _ = Alert::new()
                .title(msg.render(lang))
                .variant(functora_egui::AlertVariant::Default)
                .show(ui, |inner| {
                    _ = inner.label(msg.render(lang));
                });
            let () = ui.add_space(8.0);
        }
        if pkg_ready {
            _ = ui.label(egui::RichText::new(Msg::ArchiveReady.render(lang)).size(16.0).strong());
            let () = ui.add_space(12.0);
        } else if !url.is_empty() {
            if !qr.is_empty() {
                _ = ui.add(
                    egui::Image::from_bytes("bytes://share-qr.svg", qr.as_bytes().to_vec())
                        .maintain_aspect_ratio(true)
                        .max_width(ui.available_width().min(320.0)),
                );
                let () = ui.add_space(8.0);
            }
            _ = ui.add(
                Textarea::new(&mut url.clone())
                    .min_height(60.0)
                    .desired_width(ui.available_width()),
            );
            let () = ui.add_space(4.0);
            if ui.input(|i| i.pointer.any_click()) {
                let u = url.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(u).await.map_err(AppError::from)
                });
                self.clipboard_write_rx = Some(rx);
            }
            let () = ui.add_space(8.0);
        } else if self.temporary.message.is_some() {
            // already shown
        } else {
            _ = ui.label(Msg::Base(functora_egui::messages::Msg::Loading).render(lang));
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(0.5));
        }
        let () = ui.add_space(12.0);
        if pkg_ready || !url.is_empty() {
            _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
                if !url.is_empty() {
                    if f.add(
                        Button::new(BaseMsg::Copy.render(lang))
                            .icon(functora_egui::LucideIcon::Copy)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                    {
                        let u = url.clone();
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::clipboard::write(u).await.map_err(AppError::from)
                        });
                        self.clipboard_write_rx = Some(rx);
                    }
                    if f.add(
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
                    let bytes = match &self.temporary.external {
                        External::Archive(a) => a.clone().untag(),
                        _ => Vec::new(),
                    };
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
