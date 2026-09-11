use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::hooks::{build_external, share_error};
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use crate::route::Screen;
use crate::state::{ActionMode, AttachmentIdx, External};
use functora_egui::files::{Preview, format_size, preview};
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Alert, Button, ButtonVariant, Card, ComponentSize, Flex, Progress, Separator, ToastVariant};

impl CryptonoteApp {
    pub(crate) fn screen_view(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        if self.temporary.note.is_empty() && self.temporary.attachments.is_empty() {
            _ = Alert::new()
                .title(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)).render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |inner| {
                    _ = inner.label(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)).render(lang));
                });
            let () = ui.add_space(8.0);
        } else {
            let note = self.temporary.note.clone();
            _ = Card::new().show(ui, |inner| {
                let _ = egui::ScrollArea::vertical().max_height(400.0).show(inner, |scroll| {
                    _ = functora_egui::markdown_view::show(scroll, &mut self.md_cache, &note);
                });
            });
            let () = ui.add_space(8.0);
            if !self.temporary.attachments.is_empty() {
                _ = Separator::horizontal().show(ui);
                let () = ui.add_space(8.0);
                let attachments = self.temporary.attachments.clone();
                for (idx, att) in attachments.iter().enumerate() {
                    let size = format_size(att.data.len() as u64);
                    let name = att.name.clone();
                    _ = Flex::row().gap(8.0).show(ui, |f| {
                        let _ = f.ui(|inner| {
                            _ = inner.label(format!("{name} ({size})"));
                        });
                        if f.add(
                            Button::new(Msg::ViewButton.render(lang))
                                .icon(functora_egui::LucideIcon::Eye)
                                .size(ComponentSize::Sm),
                        )
                        .inner
                        .clicked()
                        {
                            self.temporary.attachment = Some(AttachmentIdx(idx));
                            self.navigate(Screen::File);
                        }
                        if f.add(
                            Button::new(Msg::Download.render(lang))
                                .icon(functora_egui::LucideIcon::Download)
                                .size(ComponentSize::Sm),
                        )
                        .inner
                        .clicked()
                        {
                            let att_clone = att.clone();
                            if claim_job(&mut self.temporary.progress, Stage::Download).is_some() {
                                let rx = functora_egui::spawn_async(async move {
                                    functora_egui::download::download(att_clone.data.to_vec(), &att_clone.name)
                                        .await
                                        .map_err(AppError::from)
                                });
                                self.download_rx = Some(rx);
                            }
                        }
                    });
                    let () = ui.add_space(4.0);
                    // preview via functora_egui::files::preview
                    let preview = preview(&att.name, &att.data);
                    match preview {
                        Preview::Text(t) => {
                            _ = ui.label(egui::RichText::new(t.chars().take(300).collect::<String>()).small());
                        }
                        Preview::Image(_) => {
                            let uri = format!("{}thumb-{}", crate::app::BYTES_URI_PREFIX, att.name);
                            _ = ui.add(
                                egui::Image::from_bytes(uri, att.data.to_vec())
                                    .maintain_aspect_ratio(true)
                                    .max_width(200.0),
                            );
                        }
                        _ => {}
                    }
                    let () = ui.add_space(4.0);
                }
                let () = ui.add_space(8.0);
                if ui
                    .add(
                        Button::new(Msg::DownloadAll.render(lang))
                            .icon(functora_egui::LucideIcon::Download)
                            .variant(ButtonVariant::Outline),
                    )
                    .clicked()
                {
                    let files = self.temporary.attachments.clone();
                    if claim_job(&mut self.temporary.progress, Stage::Zip).is_some() {
                        let rx = functora_egui::spawn_async(async move {
                            let zipped = functora_egui::zip::create_zip_async(&files, |_| {}, Stage::Zip).await?;
                            functora_egui::download::download(zipped, "cryptonote-unlocked.zip")
                                .await
                                .map_err(AppError::from)
                        });
                        self.download_rx = Some(rx);
                    }
                }
            }
        }
        let () = ui.add_space(12.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(
                Button::new(BaseMsg::Copy.render(lang))
                    .icon(functora_egui::LucideIcon::Copy)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let text = self.temporary.note.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await.map_err(AppError::from)
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
                if let Some(err) = share_error(self.temporary.cipher, &self.temporary.password) {
                    self.toast.add(err.render(lang), ToastVariant::Error, toast_time);
                } else if !matches!(self.temporary.external, External::Nothing) {
                    self.navigate(Screen::Share);
                } else if claim_job(&mut self.temporary.progress, Stage::Encrypt).is_some() {
                    let note = self.temporary.note.clone();
                    let password = self.temporary.password.clone();
                    let cipher = self.temporary.cipher;
                    let attachments = self.temporary.attachments.clone();
                    let rx = functora_egui::spawn_async(async move {
                        build_external(&note, &password, cipher, &attachments, |_| {}).await
                    });
                    self.generate_rx = Some(rx);
                }
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
