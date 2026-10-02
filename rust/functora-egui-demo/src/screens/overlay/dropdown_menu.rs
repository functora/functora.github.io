use super::common::ProfileAction;
use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, DropdownMenu, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_dropdown_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A menu of actions anchored to a trigger.").show(ui);
        ui.add_space(12.0);
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

        snippet(
            ui,
            "// DropdownMenu: click-triggered action menu bound to an enum\nuse functora_egui::{DropdownMenu, Button, ButtonVariant, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum ProfileAction { Profile, Settings, LogOut }\n\nlet response = Button::new(\"Open Menu\")\n    .icon(LucideIcon::ChevronDown)\n    .variant(ButtonVariant::Outline)\n    .show(ui);\n\nlet entries = [(ProfileAction::Profile, \"Profile\".to_owned()), (ProfileAction::Settings, \"Settings\".to_owned()), (ProfileAction::LogOut, \"Log out\".to_owned())];\nDropdownMenu::show_value(ui, &response, &entries, |action| {\n    match action {\n        ProfileAction::Profile => eprintln!(\"Open profile\"),\n        ProfileAction::Settings => eprintln!(\"Open settings\"),\n        ProfileAction::LogOut => eprintln!(\"Log out\"),\n    }\n});",
        );
    }
}
