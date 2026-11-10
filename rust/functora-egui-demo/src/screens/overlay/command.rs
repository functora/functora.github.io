use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_command(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "A searchable command palette over everything.");
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Command with explicit items");
        let items = vec![
            functora_egui::CommandItem {
                group: "File".to_owned(),
                group_icon: LucideIcon::File,
                label: "New File".to_owned(),
                icon: LucideIcon::FilePlus,
            },
            functora_egui::CommandItem {
                group: "Edit".to_owned(),
                group_icon: LucideIcon::Pencil,
                label: "Copy".to_owned(),
                icon: LucideIcon::Copy,
            },
        ];
        _ = Typography::small(format!(
            "Command::with_items({} items) - one icon per group and per item",
            items.len()
        ))
        .show(ui);
        if Button::new("Open Item Palette")
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.demo.command_palette.open = true;
            self.demo.command_palette.search.clear();
        }
        let ctx = ui.ctx().clone();
        if self.demo.command_palette.open
            && let Some(index) = functora_egui::Command::with_items(items)
                .placeholder("Search...")
                .show(
                    &ctx,
                    &mut self.demo.command_palette.open,
                    &mut self.demo.command_palette.search,
                )
        {
            self.toast.add(
                format!("Command item index: {index}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// CommandValue: searchable palette bound to an enum\nuse functora_egui::{CommandValue, CommandItem, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum DemoCommand { NewFile, Copy, Paste }\n\nlet entries = vec![\n    (DemoCommand::NewFile, CommandItem { group: \"File\".to_owned(), group_icon: LucideIcon::File, label: \"New File\".to_owned(), icon: LucideIcon::FilePlus }),\n    (DemoCommand::Copy, CommandItem { group: \"Edit\".to_owned(), group_icon: LucideIcon::Pencil, label: \"Copy\".to_owned(), icon: LucideIcon::Copy }),\n    (DemoCommand::Paste, CommandItem { group: \"Edit\".to_owned(), group_icon: LucideIcon::Pencil, label: \"Paste\".to_owned(), icon: LucideIcon::ClipboardPaste }),\n];\nlet mut open = false;\nlet mut search = String::new();\n\nif let Some(command) = CommandValue::new(entries)\n    .placeholder(\"Search...\")\n    .show(ctx, &mut open, &mut search)\n{\n    eprintln!(\"Selected: {command:?}\");\n}\n\n// Command: the index-based variant, with explicit group and item icons\nuse functora_egui::Command;\n\nlet items = vec![\n    CommandItem { group: \"File\".to_owned(), group_icon: LucideIcon::File, label: \"New File\".to_owned(), icon: LucideIcon::FilePlus },\n    CommandItem { group: \"Edit\".to_owned(), group_icon: LucideIcon::Pencil, label: \"Copy\".to_owned(), icon: LucideIcon::Copy },\n];\nlet mut open = false;\nlet mut search = String::new();\n\nif let Some(index) = Command::with_items(items)\n    .placeholder(\"Search...\")\n    .show(ctx, &mut open, &mut search)\n{\n    eprintln!(\"Selected index: {index}\");\n}",
        );
    }
}
