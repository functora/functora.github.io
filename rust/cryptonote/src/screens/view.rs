use crate::app::{CryptonoteApp, OpStart};
use crate::error::AppError;
use crate::hooks::{build_external, share_error};
use crate::messages::Msg;
use crate::progress::Stage;
use crate::route::Screen;
use crate::state::{ActionMode, AttachmentIdx, External};
use functora_egui::files::{Preview, format_size};
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Alert, Button, ButtonVariant, Card, ComponentSize, Flex, Separator, ToastVariant};

impl CryptonoteApp {
    pub fn screen_view(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let nothing_to_show = self.temporary.note.is_empty() && self.temporary.attachments.is_empty();
        if nothing_to_show {
            _ = Alert::new()
                .title(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)).render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |_| {});
            let () = ui.add_space(8.0);
        }
        let note = self.temporary.note.clone();
        if !note.is_empty() {
            _ = Card::new().show(ui, |inner| {
                let _ = egui::ScrollArea::vertical().max_height(400.0).show(inner, |scroll| {
                    _ = functora_egui::markdown_view::show(scroll, &mut self.md_cache, &note);
                });
            });
            let () = ui.add_space(8.0);
        }
        if !self.temporary.attachments.is_empty() {
            _ = Separator::horizontal().show(ui);
            let () = ui.add_space(8.0);
            let attachments = self.temporary.attachments.clone();
            for (idx, att) in attachments.iter().enumerate() {
                let size = format_size(att.data.len() as u64);
                let name = att.name.clone();
                _ = Flex::row().gap(8.0).wrap().single_line_items().show(ui, |f| {
                    let thumb_clicked = f
                        .ui(|inner| crate::screens::attachment_thumb::show_attachment_thumb(inner, att))
                        .inner
                        .as_ref()
                        .is_some_and(egui::Response::clicked);
                    if thumb_clicked {
                        self.temporary.attachment = Some(AttachmentIdx(idx));
                        self.navigate(Screen::File);
                    }
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
                        Button::new(Msg::Save.render(lang))
                            .icon(functora_egui::LucideIcon::Save)
                            .size(ComponentSize::Sm),
                    )
                    .inner
                    .clicked()
                    {
                        let att_clone = att.clone();
                        if self.begin_op(Stage::Download).is_some() {
                            self.download_rx = Some(functora_egui::spawn_async(async move {
                                Ok(functora_egui::download::download(att_clone.data.to_vec(), &att_clone.name).await?)
                            }));
                        }
                    }
                });
                let () = ui.add_space(4.0);
                let preview = functora_egui::files::preview_cached(&att.name, &att.data);
                match preview {
                    Preview::Text(t) => {
                        _ = ui.label(egui::RichText::new(t.chars().take(300).collect::<String>()).small());
                    }
                    Preview::Image(_) => {}
                    Preview::Markdown(t) => {
                        _ = egui::ScrollArea::vertical().max_height(200.0).show(ui, |inner| {
                            _ = functora_egui::markdown_view::show(inner, &mut self.md_cache, &t);
                        });
                    }
                    Preview::Video(_) | Preview::Audio(_) | Preview::Pdf(_) | Preview::Download => {
                        _ = ui.label(Msg::PreviewUnavailable.render(lang));
                    }
                    Preview::Missing => {
                        _ = ui.label(Msg::FileNotFound.render(lang));
                    }
                }
                let () = ui.add_space(4.0);
            }
            let () = ui.add_space(8.0);
        }
        let () = ui.add_space(12.0);
        let has_attachments = !self.temporary.attachments.is_empty();
        _ = Flex::row().gap(8.0).wrap().single_line_items().show(ui, |f| {
            self.dock_back_flex(f);
            if f.add(
                Button::new(BaseMsg::Copy.render(lang))
                    .icon(functora_egui::LucideIcon::Copy)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let text = self.temporary.note.clone();
                self.clipboard_write_rx = Some(functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await?;
                    Ok(())
                }));
            }
            if f.add(
                Button::new(Msg::Share.render(lang))
                    .icon(functora_egui::LucideIcon::Share2)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                if let Some(err) = share_error(self.temporary.cipher.into(), &self.temporary.password) {
                    self.toast.add(err.render(lang), ToastVariant::Error, toast_time);
                } else if !matches!(self.temporary.external, External::Nothing) {
                    self.navigate(Screen::Share);
                } else if let Some(OpStart { cancel, progress, .. }) = self.begin_op(Stage::Encrypt) {
                    let share_note = self.temporary.note.clone();
                    let password = self.temporary.password.clone();
                    let cipher = self.temporary.cipher.into();
                    let attachments = self.temporary.attachments.clone();
                    self.generate_rx = Some(functora_egui::spawn_async(async move {
                        build_external(&share_note, &password, cipher, &attachments, cancel, progress).await
                    }));
                }
            }
            if has_attachments
                && f.add(
                    Button::new(Msg::SaveAll.render(lang))
                        .icon(functora_egui::LucideIcon::Save)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
            {
                let files = self.temporary.attachments.clone();
                if let Some(OpStart { cancel, progress, .. }) = self.begin_op(Stage::Zip) {
                    self.download_rx = Some(functora_egui::spawn_async(async move {
                        let zipped = functora_egui::zip::create_zip_async(&files, progress, Stage::Zip, cancel).await?;
                        Ok(functora_egui::download::download(zipped, "cryptonote-unlocked.zip").await?)
                    }));
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
        });
    }
}
