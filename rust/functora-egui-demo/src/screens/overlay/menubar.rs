use super::common::{EditAction, HelpAction, ViewAction};
use functora_egui::snippet;
use functora_egui::{Menubar, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_menubar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A horizontal menu bar with dropdown menus.").show(ui);
        ui.add_space(12.0);
        let ctx = ui.ctx().clone();
        _ = Menubar::new().show(ui, |ui69| {
            _ = Menubar::item(ui69, "File");
            let edit_entries = [
                (EditAction::Undo, "Undo".to_owned()),
                (EditAction::Redo, "Redo".to_owned()),
                (EditAction::Cut, "Cut".to_owned()),
                (EditAction::Copy, "Copy".to_owned()),
                (EditAction::Paste, "Paste".to_owned()),
            ];
            Menubar::menu_value(ui69, "Edit", &edit_entries, |action| {
                self.toast.add(
                    format!("Edit menu: {action:?}"),
                    functora_egui::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            });
            let view_entries = [
                (ViewAction::ZoomIn, "Zoom In".to_owned()),
                (ViewAction::ZoomOut, "Zoom Out".to_owned()),
                (ViewAction::FullScreen, "Full Screen".to_owned()),
            ];
            Menubar::menu_value(ui69, "View", &view_entries, |action| {
                self.toast.add(
                    format!("View menu: {action:?}"),
                    functora_egui::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            });
            let help_entries = [
                (HelpAction::Documentation, "Documentation".to_owned()),
                (HelpAction::About, "About".to_owned()),
            ];
            Menubar::menu_value(ui69, "Help", &help_entries, |action| {
                self.toast.add(
                    format!("Help menu: {action:?}"),
                    functora_egui::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            });
        });

        snippet(
            ui,
            "// Menubar: menu bar with enum-bound dropdowns\nuse functora_egui::Menubar;\n\n#[derive(Clone, Copy, PartialEq)]\nenum EditAction { Undo, Redo, Cut, Copy, Paste }\n\n#[derive(Clone, Copy, PartialEq)]\nenum ViewAction { ZoomIn, ZoomOut, FullScreen }\n\n#[derive(Clone, Copy, PartialEq)]\nenum HelpAction { Documentation, About }\n\nMenubar::new().show(ui, |bar| {\n    Menubar::item(bar, \"File\");\n    let edit = [(EditAction::Undo, \"Undo\".to_owned()), (EditAction::Redo, \"Redo\".to_owned()), (EditAction::Cut, \"Cut\".to_owned()), (EditAction::Copy, \"Copy\".to_owned()), (EditAction::Paste, \"Paste\".to_owned())];\n    Menubar::menu_value(bar, \"Edit\", &edit, |action| {\n        match action {\n            EditAction::Undo => eprintln!(\"Undo\"),\n            EditAction::Redo => eprintln!(\"Redo\"),\n            EditAction::Cut => eprintln!(\"Cut\"),\n            EditAction::Copy => eprintln!(\"Copy\"),\n            EditAction::Paste => eprintln!(\"Paste\"),\n        }\n    });\n    let view = [(ViewAction::ZoomIn, \"Zoom In\".to_owned()), (ViewAction::ZoomOut, \"Zoom Out\".to_owned()), (ViewAction::FullScreen, \"Full Screen\".to_owned())];\n    Menubar::menu_value(bar, \"View\", &view, |action| {\n        eprintln!(\"View: {action:?}\");\n    });\n    let help = [(HelpAction::Documentation, \"Documentation\".to_owned()), (HelpAction::About, \"About\".to_owned())];\n    Menubar::menu_value(bar, \"Help\", &help, |action| {\n        eprintln!(\"Help: {action:?}\");\n    });\n});",
        );
    }
}
