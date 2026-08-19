use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::ActionMode;

impl CryptonoteApp {
    pub(crate) fn render_view(&mut self, ui: &mut egui::Ui) {
        self.render_note_display(ui);
        let _buttons = ui.horizontal_wrapped(|buttons| {
            if buttons.button(self.text(&Msg::EditNote)).clicked() {
                self.action = ActionMode::Create;
                self.navigate(Screen::Home);
            }
            if buttons.button(self.text(&Msg::Share)).clicked() {
                self.navigate(Screen::Share);
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }

    pub(crate) fn render_note_display(&mut self, ui: &mut egui::Ui) {
        let note = self.note.clone();
        let _scroll = egui::ScrollArea::vertical().show(ui, |scroll| {
            _ = scroll.add(
                egui::Label::new(egui::RichText::new(note).monospace())
                    .wrap()
                    .selectable(true),
            );
        });
    }
}
