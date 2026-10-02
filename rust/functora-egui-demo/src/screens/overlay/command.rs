use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_command(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A searchable command palette over everything.").show(ui);
        ui.add_space(12.0);
        if Button::new("Open Command Palette")
            .icon(LucideIcon::Command)
            .variant(ButtonVariant::Outline)
            .shortcut_text("Ctrl K")
            .show(ui)
            .clicked()
        {
            self.dialogs.command_open = true;
            self.command_search.clear();
        }
        ui.add_space(4.0);
        _ = Typography::small("Type to filter components and press Enter to jump.").show(ui);

        snippet(
            ui,
            "// CommandValue: searchable palette bound to an enum\nuse functora_egui::{CommandValue, CommandItem, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum DemoCommand { NewFile, Copy, Paste }\n\nlet entries = vec![\n    (DemoCommand::NewFile, CommandItem { group: \"File\".to_owned(), group_icon: LucideIcon::File, label: \"New File\".to_owned(), icon: LucideIcon::FilePlus }),\n    (DemoCommand::Copy, CommandItem { group: \"Edit\".to_owned(), group_icon: LucideIcon::Pencil, label: \"Copy\".to_owned(), icon: LucideIcon::Copy }),\n    (DemoCommand::Paste, CommandItem { group: \"Edit\".to_owned(), group_icon: LucideIcon::Pencil, label: \"Paste\".to_owned(), icon: LucideIcon::ClipboardPaste }),\n];\nlet mut open = false;\nlet mut search = String::new();\n\nif let Some(command) = CommandValue::new(entries)\n    .placeholder(\"Search...\")\n    .show(ctx, &mut open, &mut search)\n{\n    eprintln!(\"Selected: {command:?}\");\n}",
        );
    }
}
