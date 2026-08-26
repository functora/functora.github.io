//! Overlays: dialogs, sheets, drawers, popovers, hover cards, tooltips,
//! context menus, dropdowns, command palette, menubars, navigation menus.

use functora_egui::{
    Button, ButtonVariant, ContextMenu, DropdownMenu, Flex, HoverCard, Label, LucideIcon, Menubar,
    NavigationMenu, Popover, Tooltip, Typography,
};

use super::code::snippet;

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

        super::code::snippet(
            ui,
            "Dialog::new().title(\"Title\").show(ctx, &mut open, |ui| { ... });",
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

        super::code::snippet(
            ui,
            "AlertDialog::new(\"Are you sure?\", \"Cannot be undone.\")\n    .destructive()\n    .show(ctx, &mut open);",
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

        super::code::snippet(
            ui,
            "Sheet::new().side(SheetSide::Right).show(ctx, &mut open, |ui| { ... });",
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

        snippet(ui, "Drawer::new().show(ctx, &mut open, |ui| { ... });");
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

        snippet(ui, "Popover::new().show(ui, &response, |ui| { ... });");
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

        super::code::snippet(
            ui,
            "HoverCard::new().width(260.0).show(&response, |ui| { ... });",
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

        snippet(ui, "Tooltip::new(\"Settings\").show(&response);");
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

        super::code::snippet(
            ui,
            "ContextMenu::show(&response, &[\"Cut\", \"Copy\"], |idx| { ... });",
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

        super::code::snippet(
            ui,
            "DropdownMenu::show(ui, &response, &items, |idx| { ... });",
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

        super::code::snippet(
            ui,
            "Command::new(items).placeholder(\"Search...\")\n    .show(ctx, &mut open, &mut search);",
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

        super::code::snippet(
            ui,
            "Menubar::new().show(ui, |bar| {\n    Menubar::menu(bar, \"Edit\", &[\"Undo\", \"Redo\"], |idx| { ... });\n});",
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

        snippet(ui, "NavigationMenu::new(items).show(ui, &mut active_idx);");
    }
}
