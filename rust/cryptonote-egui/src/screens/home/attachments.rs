use crate::app::CryptonoteApp;
use crate::hooks::remove_attachment;
use crate::messages::Msg;
use crate::route::Screen;
use crate::state::AttachmentIdx;
use functora_egui::files::format_size;
use functora_egui::i18n::I18N;
use functora_egui::{Button, ButtonVariant, ComponentSize, Flex};

impl CryptonoteApp {
    pub(crate) fn show_attachments(&mut self, ui: &mut egui::Ui) {
        if self.temporary.attachments.is_empty() {
            return;
        }
        let () = ui.add_space(8.0);
        let mut to_remove: Option<AttachmentIdx> = None;
        let mut to_open: Option<AttachmentIdx> = None;
        for (idx, att) in self.temporary.attachments.iter().enumerate() {
            let size = format_size(att.data.len() as u64);
            _ = Flex::row().gap(8.0).show(ui, |f| {
                _ = f.ui(|inner| {
                    _ = inner.label(format!("{} ({})", att.name, size));
                });
                if f.add(
                    Button::new(Msg::ViewButton.render(self.lang()))
                        .icon(functora_egui::LucideIcon::Eye)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                {
                    to_open = Some(AttachmentIdx(idx));
                }
                if f.add(
                    Button::new(Msg::RemoveFile.render(self.lang()))
                        .icon(functora_egui::LucideIcon::Trash2)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                {
                    to_remove = Some(AttachmentIdx(idx));
                }
            });
            let () = ui.add_space(4.0);
        }
        if let Some(idx) = to_remove {
            remove_attachment(&mut self.temporary, idx);
        }
        if let Some(idx) = to_open {
            self.temporary.attachment = Some(idx);
            self.navigate(Screen::File);
        }
    }
}
