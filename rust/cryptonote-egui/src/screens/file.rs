use crate::app::CryptonoteApp;
use crate::messages::Msg;
use functora_core::files::format_size;

impl CryptonoteApp {
    pub(crate) fn render_file(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::File));
        if self.attachments.is_empty() {
            _ = ui.label(self.text(&Msg::FileNotFound));
            return;
        }
        let names = self
            .attachments
            .iter()
            .map(|a| (a.name.clone(), a.data.len(), a.data.to_vec()))
            .collect::<Vec<_>>();
        let _scroll = egui::ScrollArea::vertical().show(ui, |scroll| {
            for (name, size, bytes) in names {
                let _row = scroll.horizontal(|row| {
                    _ = row.label(&name);
                    _ = row.label(format_size(size as u64));
                    if row.button(self.text(&Msg::Download)).clicked() {
                        self.download(name.clone(), bytes);
                    }
                });
            }
        });
    }
}
