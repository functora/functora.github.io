use super::common::ProfileAction;
use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, DropdownMenu, LucideIcon, MenuItem};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_dropdown_menu(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "A menu of actions anchored to a trigger.");
        let response = Button::new("Open Menu")
            .icon(LucideIcon::ChevronDown)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let entries = [
            (ProfileAction::Profile, "Profile".to_owned()),
            (ProfileAction::Settings, "Settings".to_owned()),
            (ProfileAction::LogOut, "Log out".to_owned()),
        ];
        let ctx = ui.ctx().clone();
        DropdownMenu::show_value(ui, &response, &entries, |action| {
            self.toast.add(
                format!("Dropdown menu: {action:?}"),
                functora_egui::ToastVariant::Default,
                ctx.input(|i| i.time),
            );
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Rich menu (shortcuts, separators, disabled items)");
        let rich_response = Button::new("Open Rich Menu")
            .icon(LucideIcon::ChevronDown)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let rich_items = [
            MenuItem::label("Plain item"),
            MenuItem::with_shortcut("Save", "Ctrl+S"),
            MenuItem::label("Checked item").selected(true),
            MenuItem::full("Disabled item", Some("Ctrl+D".to_owned()), false),
            MenuItem::separator(),
            MenuItem::with_shortcut("Quit", "Ctrl+Q"),
        ];
        DropdownMenu::show_rich(ui, &rich_response, &rich_items, |index| {
            self.toast.add(
                format!("Rich menu item: {index}"),
                functora_egui::ToastVariant::Default,
                ctx.input(|i| i.time),
            );
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Rich menu bound to enum values");
        let value_response = Button::new("Open Value Menu")
            .icon(LucideIcon::ChevronDown)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let value_items = [
            (ProfileAction::Profile, MenuItem::label("Profile")),
            (
                ProfileAction::Settings,
                MenuItem::with_shortcut("Settings", "Ctrl+,"),
            ),
            (ProfileAction::LogOut, MenuItem::separator()),
        ];
        DropdownMenu::show_rich_value(ui, &value_response, &value_items, |action| {
            self.toast.add(
                format!("Value menu: {action:?}"),
                functora_egui::ToastVariant::Default,
                ctx.input(|i| i.time),
            );
        });

        snippet(
            ui,
            "// DropdownMenu: click-triggered action menu bound to an enum\nuse functora_egui::{DropdownMenu, Button, ButtonVariant, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum ProfileAction { Profile, Settings, LogOut }\n\nlet response = Button::new(\"Open Menu\")\n    .icon(LucideIcon::ChevronDown)\n    .variant(ButtonVariant::Outline)\n    .show(ui);\n\nlet entries = [(ProfileAction::Profile, \"Profile\".to_owned()), (ProfileAction::Settings, \"Settings\".to_owned()), (ProfileAction::LogOut, \"Log out\".to_owned())];\nDropdownMenu::show_value(ui, &response, &entries, |action| {\n    match action {\n        ProfileAction::Profile => eprintln!(\"Open profile\"),\n        ProfileAction::Settings => eprintln!(\"Open settings\"),\n        ProfileAction::LogOut => eprintln!(\"Log out\"),\n    }\n});\n\n// Rich menu: shortcuts, check marks, separators and disabled items\nuse functora_egui::MenuItem;\n\nlet items = [\n    MenuItem::label(\"Plain item\"),\n    MenuItem::with_shortcut(\"Save\", \"Ctrl+S\"),\n    MenuItem::label(\"Checked item\").selected(true),\n    MenuItem::full(\"Disabled item\", Some(\"Ctrl+D\".to_owned()), false),\n    MenuItem::separator(),\n];\nDropdownMenu::show_rich(ui, &response, &items, |index| eprintln!(\"item {index}\"));\n\n// Rich menu bound to enum values: each entry pairs a value with a MenuItem\nlet value_items = [\n    (ProfileAction::Profile, MenuItem::label(\"Profile\")),\n    (ProfileAction::Settings, MenuItem::with_shortcut(\"Settings\", \"Ctrl+,\")),\n];\nDropdownMenu::show_rich_value(ui, &response, &value_items, |action| {\n    eprintln!(\"{action:?}\");\n});",
        );
    }
}
