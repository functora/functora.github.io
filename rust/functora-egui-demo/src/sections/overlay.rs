//! Overlays: dialogs, sheets, drawers, popovers, hover cards, tooltips,
//! context menus, dropdowns, command palette, menubars, navigation menus.

use functora_egui::{
    Button, ButtonVariant, ContextMenu, DropdownMenu, Flex, HoverCard, Label, LucideIcon, Menubar,
    NavigationMenu, Popover, Tooltip, Typography,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_dialog(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A modal dialog with a backdrop.").show(ui);
        ui.add_space(12.0);
        if Button::new("Open Dialog")
            .icon(LucideIcon::SquareMenu)
            .show(ui)
            .clicked()
        {
            self.dialogs.dialog_open = true;
        }

        snippet(
            ui,
            "// Dialog: modal dialog with backdrop\nuse functora_egui::{Dialog, Button, ButtonVariant, LucideIcon};\n\nlet mut open = false;\n\nif Button::new(\"Open Dialog\").icon(LucideIcon::AppWindow).show(ui).clicked() {\n    open = true;\n}\n\nDialog::new()\n    .title(\"Edit Profile\")\n    .description(\"Make changes to your profile here.\")\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Full name\").show(ui);\n        Input::new(&mut name).placeholder(\"Ada Lovelace\").show(ui);\n        ui.add_space(8.0);\n        Label::new(\"Bio\").show(ui);\n        Textarea::new(&mut bio).placeholder(\"Tell us...\").show(ui);\n        ui.add_space(12.0);\n        Flex::row().justify_end().gap(8.0).show(ui, |f| {\n            f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline));\n            if f.add(Button::new(\"Save\").icon(LucideIcon::Check)).clicked() {\n                open = false;\n            }\n        });\n    });",
        );
    }

    pub(crate) fn demo_alert_dialog(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A confirmation dialog with a destructive action.").show(ui);
        ui.add_space(12.0);
        if Button::new("Delete Account")
            .icon(LucideIcon::Trash)
            .variant(ButtonVariant::Destructive)
            .show(ui)
            .clicked()
        {
            self.dialogs.alert_dialog_open = true;
        }

        snippet(
            ui,
            "// AlertDialog: confirmation with destructive action\nuse functora_egui::{AlertDialog, Button, ButtonVariant, LucideIcon};\n\nlet mut open = false;\n\nif Button::new(\"Delete Account\").variant(ButtonVariant::Destructive).show(ui).clicked() {\n    open = true;\n}\n\nlet result = AlertDialog::new(\n    \"Are you absolutely sure?\",\n    \"This action cannot be undone.\"\n)\n.destructive()\n.show(ctx, &mut open);\n\nmatch result {\n    AlertDialogResult::Confirmed => eprintln!(\"User confirmed deletion\"),\n    AlertDialogResult::Cancelled => eprintln!(\"User cancelled\"),\n    _ => {}\n}",
        );
    }

    pub(crate) fn demo_sheet(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A side panel that slides in from the edge.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("On mobile the sheet opens from the bottom.").show(ui);
        ui.add_space(4.0);
        if Button::new("Open Sheet")
            .icon(LucideIcon::PanelRight)
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.drawers.sheet_open = true;
        }

        snippet(
            ui,
            "// Sheet: side panel from edge (right/left/top/bottom)\nuse functora_egui::{Sheet, SheetSide, Button, ButtonVariant, LucideIcon, Label};\n\nlet mut open = false;\n\nif Button::new(\"Open Sheet\").icon(LucideIcon::PanelRight).show(ui).clicked() {\n    open = true;\n}\n\nSheet::new()\n    .title(\"Sheet Panel\")\n    .description(\"A side sheet that slides in from the edge.\")\n    .side(SheetSide::Right)\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Notifications\").show(ui);\n        // ... content\n    });\n\n// On mobile, opens from bottom regardless of side",
        );
    }

    pub(crate) fn demo_drawer(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A panel that slides up from the bottom edge.").show(ui);
        ui.add_space(12.0);
        if Button::new("Open Drawer")
            .icon(LucideIcon::PanelBottomOpen)
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.drawers.drawer_open = true;
        }

        snippet(
            ui,
            "// Drawer: bottom panel (mobile) or side panel (desktop)\nuse functora_egui::{Drawer, Button, ButtonVariant, LucideIcon, FieldDescription};\n\nlet mut open = false;\n\nif Button::new(\"Open Drawer\").icon(LucideIcon::PanelBottomOpen).show(ui).clicked() {\n    open = true;\n}\n\nDrawer::new()\n    .title(\"Drawer\")\n    .description(\"A bottom drawer panel.\")\n    .show(ctx, &mut open, |ui| {\n        FieldDescription::show(ui, \"On mobile, drawers slide up from bottom.\");\n        Flex::row().justify_end().gap(8.0).show(ui, |f| {\n            f.add(Button::new(\"Close\").variant(ButtonVariant::Outline));\n        });\n    });",
        );
    }

    pub(crate) fn demo_popover(ui: &mut egui::Ui) {
        _ = Typography::muted("A floating popup anchored to a trigger.").show(ui);
        ui.add_space(12.0);
        let response = Button::new("Open Popover")
            .icon(LucideIcon::PanelTopOpen)
            .variant(ButtonVariant::Outline)
            .show(ui);
        Popover::new().show(ui, &response, |ui68| {
            _ = Label::new("Popover content").show(ui68);
            _ = ui68.label("Click the button again to close it.");
        });

        snippet(
            ui,
            "// Popover: floating popup anchored to trigger\nuse functora_egui::{Popover, Button, ButtonVariant, LucideIcon, Label};\n\nlet response = Button::new(\"Open Popover\").icon(LucideIcon::PanelTopOpen).show(ui);\n\nPopover::new().show(ui, &response, |ui| {\n    Label::new(\"Popover content\").show(ui);\n    ui.label(\"Click the button again to close it.\");\n});",
        );
    }

    pub(crate) fn demo_hover_card(ui: &mut egui::Ui) {
        _ = Typography::muted("A rich tooltip shown on hover.").show(ui);
        ui.add_space(12.0);
        let response = Button::new("Hover me")
            .icon(LucideIcon::MousePointer2)
            .variant(ButtonVariant::Outline)
            .show(ui);
        HoverCard::new().width(260.0).show(&response, |ui78| {
            _ = Typography::h4("shadcn/ui").show(ui78);
            ui78.add_space(4.0);
            _ = ui78.label(
                "Beautifully designed components that you can copy and paste into your apps.",
            );
            ui78.add_space(6.0);
            _ = Label::new("Learn more about functora-egui").show(ui78);
        });

        snippet(
            ui,
            "// HoverCard: rich tooltip on hover\nuse functora_egui::{HoverCard, Button, ButtonVariant, LucideIcon, Typography, Label};\n\nlet response = Button::new(\"Hover me\").icon(LucideIcon::MousePointer2).variant(ButtonVariant::Outline).show(ui);\n\nHoverCard::new().width(260.0).show(&response, |ui| {\n    Typography::h4(\"shadcn/ui\").show(ui);\n    ui.add_space(4.0);\n    ui.label(\"Beautifully designed components for your apps.\");\n    ui.add_space(6.0);\n    Label::new(\"Learn more about functora-egui\").show(ui);\n});",
        );
    }

    pub(crate) fn demo_tooltip(ui: &mut egui::Ui) {
        _ = Typography::muted("A small hint shown on hover.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let settings = f.add(
                Button::icon_only(LucideIcon::Settings)
                    .variant(ButtonVariant::Outline)
                    .size(functora_egui::ComponentSize::Sm),
            );
            Tooltip::new("Settings").show(&settings.response);
            let notifications = f.add(
                Button::icon_only(LucideIcon::Bell)
                    .variant(ButtonVariant::Outline)
                    .size(functora_egui::ComponentSize::Sm),
            );
            Tooltip::new("Notifications").show(&notifications.response);
        });

        snippet(
            ui,
            "// Tooltip: small hint on hover\nuse functora_egui::{Tooltip, Button, ButtonVariant, LucideIcon, ComponentSize};\n\nlet settings = Button::icon_only(LucideIcon::Settings)\n    .variant(ButtonVariant::Outline)\n    .size(ComponentSize::Sm)\n    .show(ui);\nTooltip::new(\"Settings\").show(&settings.response);\n\nlet notifications = Button::icon_only(LucideIcon::Bell)\n    .variant(ButtonVariant::Outline)\n    .size(ComponentSize::Sm)\n    .show(ui);\nTooltip::new(\"Notifications\").show(&notifications.response);",
        );
    }

    pub(crate) fn demo_context_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Right-click a target to open a context menu.").show(ui);
        ui.add_space(12.0);
        let response = Button::new("Right-click me")
            .icon(LucideIcon::MousePointerClick)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let items = ["Cut", "Copy", "Paste", "Select All"];
        ContextMenu::show(&response, &items, |idx| {
            self.toast.add(
                format!("Context menu: {}", items[idx]),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        });

        snippet(
            ui,
            "// ContextMenu: right-click menu\nuse functora_egui::{ContextMenu, Button, ButtonVariant, LucideIcon};\n\nlet response = Button::new(\"Right-click me\")\n    .icon(LucideIcon::MousePointerClick)\n    .variant(ButtonVariant::Outline)\n    .show(ui);\n\nlet items = [\"Cut\", \"Copy\", \"Paste\", \"Select All\"];\nContextMenu::show(&response, &items, |idx| {\n    match items[idx] {\n        \"Cut\" => eprintln!(\"Cut\"),\n        \"Copy\" => eprintln!(\"Copy\"),\n        _ => {}\n    }\n});",
        );
    }

    pub(crate) fn demo_dropdown_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A menu of actions anchored to a trigger.").show(ui);
        ui.add_space(12.0);
        let response = Button::new("Open Menu")
            .icon(LucideIcon::ChevronDown)
            .variant(ButtonVariant::Outline)
            .show(ui);
        let items = ["Profile", "Settings", "Log out"];
        let ctx = ui.ctx().clone();
        DropdownMenu::show(ui, &response, &items, |idx| {
            self.toast.add(
                format!("Dropdown menu: {}", items[idx]),
                functora_egui::ToastVariant::Default,
                ctx.input(|i| i.time),
            );
        });

        snippet(
            ui,
            "// DropdownMenu: click-triggered action menu\nuse functora_egui::{DropdownMenu, Button, ButtonVariant, LucideIcon};\n\nlet response = Button::new(\"Open Menu\")\n    .icon(LucideIcon::ChevronDown)\n    .variant(ButtonVariant::Outline)\n    .show(ui);\n\nlet items = [\"Profile\", \"Settings\", \"Log out\"];\nDropdownMenu::show(ui, &response, &items, |idx| {\n    match items[idx] {\n        \"Profile\" => eprintln!(\"Open profile\"),\n        \"Settings\" => eprintln!(\"Open settings\"),\n        \"Log out\" => eprintln!(\"Log out\"),\n        _ => {}\n    }\n});",
        );
    }

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
            "// Command: searchable command palette\nuse functora_egui::{Command, LucideIcon};\n\nlet items: Vec<(String, String)> = vec![\n    (\"File\".to_owned(), \"New File\".to_owned()),\n    (\"Edit\".to_owned(), \"Copy\".to_owned()),\n    (\"Edit\".to_owned(), \"Paste\".to_owned()),\n];\nlet mut open = false;\nlet mut search = String::new();\n\nif let Some(idx) = Command::new(items)\n    .placeholder(\"Search...\")\n    .show(ctx, &mut open, &mut search)\n{\n    eprintln!(\"Selected: {}\", items[idx].1);\n}",
        );
    }

    pub(crate) fn demo_menubar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A horizontal menu bar with dropdown menus.").show(ui);
        ui.add_space(12.0);
        let ctx = ui.ctx().clone();
        _ = Menubar::new().show(ui, |ui69| {
            _ = Menubar::item(ui69, "File");
            Menubar::menu(
                ui69,
                "Edit",
                &["Undo", "Redo", "Cut", "Copy", "Paste"],
                |idx| {
                    self.toast.add(
                        format!(
                            "Edit menu: {}",
                            ["Undo", "Redo", "Cut", "Copy", "Paste"][idx]
                        ),
                        functora_egui::ToastVariant::Default,
                        ctx.input(|i| i.time),
                    );
                },
            );
            Menubar::menu(
                ui69,
                "View",
                &["Zoom In", "Zoom Out", "Full Screen"],
                |idx| {
                    self.toast.add(
                        format!("View menu: {}", ["Zoom In", "Zoom Out", "Full Screen"][idx]),
                        functora_egui::ToastVariant::Default,
                        ctx.input(|i| i.time),
                    );
                },
            );
            Menubar::menu(ui69, "Help", &["Documentation", "About"], |idx| {
                self.toast.add(
                    format!("Help menu: {}", ["Documentation", "About"][idx]),
                    functora_egui::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            });
        });

        snippet(
            ui,
            "// Menubar: horizontal menu bar with dropdowns\nuse functora_egui::{Menubar, Button, LucideIcon, ToastVariant};\n\nMenubar::new().show(ui, |bar| {\n    Menubar::item(bar, \"File\");\n    Menubar::menu(bar, \"Edit\", &[\"Undo\", \"Redo\", \"Cut\", \"Copy\", \"Paste\"], |idx| {\n        match idx {\n            0 => eprintln!(\"Undo\"),\n            1 => eprintln!(\"Redo\"),\n            _ => {}\n        }\n    });\n    Menubar::menu(bar, \"View\", &[\"Zoom In\", \"Zoom Out\", \"Full Screen\"], |idx| {\n        // ...\n    });\n    Menubar::menu(bar, \"Help\", &[\"Documentation\", \"About\"], |idx| {\n        // ...\n    });\n});",
        );
    }

    pub(crate) fn demo_navigation_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Top-level navigation with active item tracking.").show(ui);
        ui.add_space(12.0);
        let clicked = NavigationMenu::new(vec![
            "Overview".to_owned(),
            "Integrations".to_owned(),
            "Settings".to_owned(),
        ])
        .show(ui, &mut self.tabs_idx);
        if let Some(idx) = clicked {
            self.toast.add(
                format!("Navigation: item {idx}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// NavigationMenu: top-level navigation with active tracking\nuse functora_egui::NavigationMenu;\n\nlet items = vec![\"Overview\", \"Integrations\", \"Settings\"];\nlet mut active = 0;\n\nif let Some(idx) = NavigationMenu::new(items).show(ui, &mut active) {\n    eprintln!(\"Navigated to: {}\", idx);\n}",
        );
    }
}
