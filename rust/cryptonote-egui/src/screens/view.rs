use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::ActionMode;
use egui_shadcn::{Button, ButtonVariant, Card, LucideIcon};

impl CryptonoteApp {
    pub(crate) fn render_view(&mut self, ui: &mut egui::Ui) {
        self.render_note_display(ui);
        let edit_label = self.text(&Msg::EditNote);
        let share_label = self.text(&Msg::Share);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&share_label).icon(LucideIcon::Share2))
                .clicked()
            {
                app.navigate(Screen::Share);
            }
            if row
                .add(
                    Button::new(&edit_label)
                        .icon(LucideIcon::Pencil)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.action = ActionMode::Create;
                app.navigate(Screen::Home);
            }
            if row
                .add(
                    Button::new(&reset_label)
                        .icon(LucideIcon::Trash2)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
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
