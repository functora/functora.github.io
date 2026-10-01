//! Overlays: dialogs, sheets, popovers, hover cards, tooltips,
//! context menus, dropdowns, command palette, menubars, navigation menus.

use crate::app::NavSection;
use functora_egui::progress::{Job, Stage};
use functora_egui::{
    BlockingOverlay, Button, ButtonVariant, ContextMenu, DropdownMenu, Flex, HoverCard, Label,
    LucideIcon, Menubar, NavigationMenuValue, Popover, SheetSide, Tooltip, Typography,
};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use functora_egui::snippet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContextAction {
    Cut,
    Copy,
    Paste,
    SelectAll,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProfileAction {
    Profile,
    Settings,
    LogOut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EditAction {
    Undo,
    Redo,
    Cut,
    Copy,
    Paste,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ViewAction {
    ZoomIn,
    ZoomOut,
    FullScreen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HelpAction {
    Documentation,
    About,
}

impl crate::app::ShowcaseApp {
    pub fn demo_dialog(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A modal dialog with a backdrop.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("On mobile the dialog opens as a bottom sheet.").show(ui);
        ui.add_space(4.0);
        if Button::new("Open Dialog")
            .icon(LucideIcon::SquareMenu)
            .show(ui)
            .clicked()
        {
            self.dialogs.dialog_open = true;
        }
        ui.add_space(12.0);
        _ = Typography::small(
            "Shrink the window below 800px, then open the dialog: it slides up from the \
             bottom instead of centering.",
        )
        .show(ui);

        snippet(
            ui,
            "// Dialog: modal dialog with backdrop\n// On mobile Dialog anchors CENTER_BOTTOM as a bottom sheet.\nuse functora_egui::{Dialog, Button, ButtonVariant, LucideIcon, ComponentSize, Label, Input, Textarea, Flex};\n\nlet mut open = false;\n\nif Button::new(\"Open Dialog\").icon(LucideIcon::SquareMenu).show(ui).clicked() {\n    open = true;\n}\n\nDialog::new()\n    .title(\"Edit Profile\")\n    .description(\"Make changes to your profile here.\")\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Full name\").show(ui);\n        ui.add_space(8.0);\n        Input::new(&mut name).placeholder(\"Ada Lovelace\").show(ui);\n        ui.add_space(8.0);\n        Label::new(\"Bio\").show(ui);\n        ui.add_space(8.0);\n        Textarea::new(&mut bio).placeholder(\"Tell us about yourself...\").desired_width(ui.available_width()).show(ui);\n        ui.add_space(12.0);\n        Flex::row().justify_end().gap(8.0).show(ui, |f| {\n            f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline).size(ComponentSize::Sm));\n            if f.add(Button::new(\"Save Changes\").size(ComponentSize::Sm).icon(LucideIcon::Check)).clicked() {\n                open = false;\n            }\n        });\n    });",
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
            "// AlertDialog: confirmation with destructive action\nuse functora_egui::{AlertDialog, AlertDialogResult, Button, ButtonVariant, LucideIcon};\n\nlet mut open = false;\n\nif Button::new(\"Delete Account\").icon(LucideIcon::Trash).variant(ButtonVariant::Destructive).show(ui).clicked() {\n    open = true;\n}\n\nlet result = AlertDialog::new(\n    \"Are you absolutely sure?\",\n    \"This action cannot be undone. This will permanently delete your account.\"\n)\n.destructive()\n.show(ctx, &mut open);\n\nmatch result {\n    AlertDialogResult::Confirmed => eprintln!(\"User confirmed deletion\"),\n    AlertDialogResult::Cancelled => eprintln!(\"User cancelled\"),\n}",
        );
    }

    pub fn demo_sheet(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A side panel that slides in from the edge.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("On mobile the sheet opens from the bottom.").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(
                Button::new("Right")
                    .icon(LucideIcon::PanelRight)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Right;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Left")
                    .icon(LucideIcon::PanelLeft)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Left;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Top")
                    .icon(LucideIcon::PanelTop)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Top;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Bottom")
                    .icon(LucideIcon::PanelBottom)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Bottom;
                self.sheet_state.sheet_open = true;
            }
        });

        snippet(
            ui,
            "// Sheet: side panel from edge (right/left/top/bottom)\nuse functora_egui::{Sheet, SheetSide, Button, ButtonVariant, LucideIcon, Label, Item, FieldDescription, Flex};\n\nlet mut open = false;\nlet mut side = SheetSide::Right;\n\nFlex::row().gap(8.0).show(ui, |f| {\n    if f.add(Button::new(\"Right\").icon(LucideIcon::PanelRight).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Right;\n        open = true;\n    }\n    if f.add(Button::new(\"Left\").icon(LucideIcon::PanelLeft).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Left;\n        open = true;\n    }\n    if f.add(Button::new(\"Top\").icon(LucideIcon::PanelTop).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Top;\n        open = true;\n    }\n    if f.add(Button::new(\"Bottom\").icon(LucideIcon::PanelBottom).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Bottom;\n        open = true;\n    }\n});\n\nSheet::new()\n    .title(\"Sheet Panel\")\n    .description(\"A side sheet that slides in from the edge.\")\n    .side(side)\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Notifications\").show(ui);\n        ui.add_space(4.0);\n        for (label, desc) in [\n            (\"New comment\", \"Alice commented on your post.\"),\n            (\"Build passed\", \"The release pipeline finished.\"),\n            (\"Update ready\", \"functora-egui 0.2 is available.\"),\n        ] {\n            Item::new().show(ui, |item| {\n                item.vertical(|v| {\n                    Label::new(label).show(v);\n                    FieldDescription::show(v, desc);\n                });\n            });\n        }\n    });\n\n// On mobile, opens from bottom regardless of side",
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
            "// Popover: floating popup anchored to trigger\nuse functora_egui::{Popover, Button, ButtonVariant, LucideIcon, Label};\n\nlet response = Button::new(\"Open Popover\").icon(LucideIcon::PanelTopOpen).variant(ButtonVariant::Outline).show(ui);\n\nPopover::new().show(ui, &response, |ui| {\n    Label::new(\"Popover content\").show(ui);\n    ui.label(\"Click the button again to close it.\");\n});",
        );
    }

    pub fn demo_hover_card(ui: &mut egui::Ui) {
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
            "// HoverCard: rich tooltip on hover\nuse functora_egui::{HoverCard, Button, ButtonVariant, LucideIcon, Typography, Label};\n\nlet response = Button::new(\"Hover me\").icon(LucideIcon::MousePointer2).variant(ButtonVariant::Outline).show(ui);\n\nHoverCard::new().width(260.0).show(&response, |ui| {\n    Typography::h4(\"shadcn/ui\").show(ui);\n    ui.add_space(4.0);\n    ui.label(\"Beautifully designed components that you can copy and paste into your apps.\");\n    ui.add_space(6.0);\n    Label::new(\"Learn more about functora-egui\").show(ui);\n});",
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
            Tooltip::new("Settings").show(&settings.inner);
            let notifications = f.add(
                Button::icon_only(LucideIcon::Bell)
                    .variant(ButtonVariant::Outline)
                    .size(functora_egui::ComponentSize::Sm),
            );
            Tooltip::new("Notifications").show(&notifications.inner);
        });

        snippet(
            ui,
            "// Tooltip: small hint on hover\nuse functora_egui::{Tooltip, Button, ButtonVariant, LucideIcon, ComponentSize, Flex};\n\nFlex::row().gap(8.0).show(ui, |f| {\n    let settings = f.add(\n        Button::icon_only(LucideIcon::Settings)\n            .variant(ButtonVariant::Outline)\n            .size(ComponentSize::Sm),\n    );\n    Tooltip::new(\"Settings\").show(&settings.inner);\n    let notifications = f.add(\n        Button::icon_only(LucideIcon::Bell)\n            .variant(ButtonVariant::Outline)\n            .size(ComponentSize::Sm),\n    );\n    Tooltip::new(\"Notifications\").show(&notifications.inner);\n});",
        );
    }

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

    pub(crate) fn demo_navigation_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Top-level navigation with active item tracking.").show(ui);
        ui.add_space(12.0);
        let entries = [
            (NavSection::Overview, "Overview".to_owned()),
            (NavSection::Integrations, "Integrations".to_owned()),
            (NavSection::Settings, "Settings".to_owned()),
        ];
        let clicked = NavigationMenuValue::new(&entries).show(ui, &mut self.nav_section);
        if let Some(section) = clicked {
            self.toast.add(
                format!("Navigation: {section:?}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// NavigationMenuValue: top-level navigation bound to an enum\nuse functora_egui::NavigationMenuValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum NavSection { Overview, Integrations, Settings }\n\nlet entries = [(NavSection::Overview, \"Overview\".to_owned()), (NavSection::Integrations, \"Integrations\".to_owned()), (NavSection::Settings, \"Settings\".to_owned())];\nlet mut active = NavSection::Overview;\n\nif let Some(section) = NavigationMenuValue::new(&entries).show(ui, &mut active) {\n    eprintln!(\"Navigated to: {section:?}\");\n}",
        );
    }

    pub fn demo_blocking_overlay(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A modal overlay that blocks interaction during long operations.")
            .show(ui);
        ui.add_space(12.0);
        if Button::new("Show Blocking Overlay")
            .icon(LucideIcon::Hourglass)
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.demo.blocking_overlay_open = true;
            self.blocking_overlay_cancel = Arc::new(AtomicBool::new(false));
            self.blocking_overlay_job = Some(Job {
                stage: Stage::Download,
                done: 45,
                total: 100,
                name: Some("archive.zip".to_owned()),
            });
        }
        ui.add_space(4.0);
        _ = Typography::small("Click Cancel inside the overlay to dismiss it.").show(ui);

        if self.demo.blocking_overlay_open {
            let mut open = self.demo.blocking_overlay_open;
            BlockingOverlay::new("Processing files...")
                .description("Reading and compressing files, please wait.")
                .show(
                    ui.ctx(),
                    &mut open,
                    self.blocking_overlay_job.as_ref(),
                    &self.blocking_overlay_cancel,
                );
            if self.blocking_overlay_cancel.load(Ordering::Relaxed) {
                open = false;
                self.blocking_overlay_job = None;
            }
            self.demo.blocking_overlay_open = open;
        }

        snippet(
            ui,
            "// BlockingOverlay: modal overlay for long operations\nuse functora_egui::BlockingOverlay;\nuse functora_egui::progress::{Job, Stage};\nuse std::sync::Arc;\nuse std::sync::atomic::{AtomicBool, Ordering};\n\nlet mut open = false;\nlet cancel = Arc::new(AtomicBool::new(false));\nlet job = Job { stage: Stage::Download, done: 45, total: 100, name: Some(\"archive.zip\".to_owned()) };\n\nBlockingOverlay::new(\"Processing files...\")\n    .description(\"Reading and compressing files, please wait.\")\n    .show(ctx, &mut open, Some(&job), &cancel);\n\n// Close when cancelled\nif cancel.load(Ordering::Relaxed) {\n    open = false;\n}",
        );
    }
}
