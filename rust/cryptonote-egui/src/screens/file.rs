use crate::app::CryptonoteApp;
use crate::messages::Msg;
use functora_egui::{Button, ButtonVariant, Card, ComponentSize, LucideIcon};
use functora_core::files::format_size;

impl CryptonoteApp {
    pub(crate) fn render_file(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::File);
        if self.attachments.is_empty() {
            let _ = Card::new().heading(heading).show(ui, |card| {
                _ = card.label(self.text(&Msg::FileNotFound));
            });
            return;
        }
        let rows = self
            .attachments
            .iter()
            .map(|a| (a.name.clone(), a.data.len(), a.data.to_vec()))
            .collect::<Vec<_>>();
        let _ = Card::new().heading(heading).show(ui, |card| {
            let _scroll = egui::ScrollArea::vertical().show(card, |scroll| {
                for (name, size, bytes) in rows {
                    let _row = scroll.horizontal_wrapped(|row| {
                        _ = row.label(egui::RichText::new(&name).strong());
                        _ = row.label(format_size(size as u64));
                        if row
                            .add(
                                Button::icon_only(LucideIcon::Download)
                                    .variant(ButtonVariant::Outline)
                                    .size(ComponentSize::Sm),
                            )
                            .on_hover_text(self.text(&Msg::Download))
                            .clicked()
                        {
                            self.download(name.clone(), bytes);
                        }
                    });
                }
            });
        });
    }
}
