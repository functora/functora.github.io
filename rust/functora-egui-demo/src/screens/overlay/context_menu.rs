use super::common::ContextAction;
use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, ContextMenu, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_context_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Right-click a target to open a context menu.").show(ui);
        ui.add_space(12.0);
        let response = Button::new("Right-click me")
            .icon(LucideIcon::MousePointerClick)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let entries = [
            (ContextAction::Cut, "Cut".to_owned()),
            (ContextAction::Copy, "Copy".to_owned()),
            (ContextAction::Paste, "Paste".to_owned()),
            (ContextAction::SelectAll, "Select All".to_owned()),
        ];
        ContextMenu::show_value(&response, &entries, |action| {
            self.toast.add(
                format!("Context menu: {action:?}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        });

        snippet(
            ui,
            "// ContextMenu: right-click menu bound to an enum\nuse functora_egui::{ContextMenu, Button, ButtonVariant, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum ContextAction { Cut, Copy, Paste, SelectAll }\n\nlet response = Button::new(\"Right-click me\")\n    .icon(LucideIcon::MousePointerClick)\n    .variant(ButtonVariant::Outline)\n    .show(ui);\n\nlet entries = [(ContextAction::Cut, \"Cut\".to_owned()), (ContextAction::Copy, \"Copy\".to_owned()), (ContextAction::Paste, \"Paste\".to_owned()), (ContextAction::SelectAll, \"Select All\".to_owned())];\nContextMenu::show_value(&response, &entries, |action| {\n    match action {\n        ContextAction::Cut => eprintln!(\"Cut\"),\n        ContextAction::Copy => eprintln!(\"Copy\"),\n        ContextAction::Paste => eprintln!(\"Paste\"),\n        ContextAction::SelectAll => eprintln!(\"Select All\"),\n    }\n});",
        );
    }
}
