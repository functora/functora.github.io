use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::ActionMode;
use elegance::glyphs;
use elegance::{Accent, Button, Card};

impl CryptonoteApp {
    pub(crate) fn render_view(&mut self, ui: &mut egui::Ui) {
        self.render_note_display(ui);
        let edit_label = format!("{} {}", glyphs::PENCIL, self.text(&Msg::EditNote));
        let share_label = format!("{} {}", glyphs::NETWORK, self.text(&Msg::Share));
        let reset_label = format!("{} {}", glyphs::TRASH, self.text(&Msg::CreateNewNote));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&share_label).accent(Accent::Blue))
                .clicked()
            {
                app.navigate(Screen::Share);
            }
            if row.add(Button::new(&edit_label).outline()).clicked() {
                app.action = ActionMode::Create;
                app.navigate(Screen::Home);
            }
            if row.add(Button::new(&reset_label).outline()).clicked() {
                app.reset();
            }
        });
    }

    pub(crate) fn render_note_display(&mut self, ui: &mut egui::Ui) {
        let _ = Card::new().show(ui, |card| {
            let _scroll = egui::ScrollArea::vertical().show(card, |scroll| {
                _ = scroll.add(
                    egui::Label::new(egui::RichText::new(&self.note).monospace())
                        .wrap()
                        .selectable(true),
                );
            });
        });
    }
}
