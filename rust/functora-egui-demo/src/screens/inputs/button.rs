use functora_egui::snippet;
use functora_egui::{
    Button, ButtonGroup, ButtonVariant, ComponentSize, Flex, LucideIcon, Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_button(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable buttons with variant styles and sizes.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Variants").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            _ = f.add(Button::new("Default"));
            _ = f.add(Button::new("Destructive").variant(ButtonVariant::Destructive));
            _ = f.add(Button::new("Outline").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Secondary").variant(ButtonVariant::Secondary));
            _ = f.add(Button::new("Ghost").variant(ButtonVariant::Ghost));
            _ = f.add(Button::new("Link").variant(ButtonVariant::Link));
        });

        ui.add_space(12.0);
        _ = Typography::small("Sizes").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(Button::new("XS").size(ComponentSize::Xs));
            _ = f.add(Button::new("Small").size(ComponentSize::Sm));
            _ = f.add(Button::new("Default"));
            _ = f.add(Button::new("Large").size(ComponentSize::Lg));
        });

        ui.add_space(12.0);
        _ = Typography::small("Icon only").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::icon_only(LucideIcon::Plus));
            _ = f.add(Button::icon_only(LucideIcon::Settings).variant(ButtonVariant::Outline));
            _ = f.add(Button::icon_only(LucideIcon::Trash).variant(ButtonVariant::Destructive));
            _ = f.add(Button::icon_only(LucideIcon::Heart).variant(ButtonVariant::Ghost));
            _ = f.add(
                Button::icon_only(LucideIcon::Search)
                    .variant(ButtonVariant::Secondary)
                    .size(ComponentSize::Sm),
            );
            _ = f.add(
                Button::icon_only(LucideIcon::Star)
                    .variant(ButtonVariant::Outline)
                    .size(ComponentSize::Lg),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Icon + text").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Download").icon(LucideIcon::Download));
            _ = f.add(
                Button::new("Upload")
                    .icon(LucideIcon::Upload)
                    .variant(ButtonVariant::Outline),
            );
            _ = f.add(
                Button::new("Mail")
                    .icon(LucideIcon::Mail)
                    .variant(ButtonVariant::Secondary),
            );
            _ = f.add(
                Button::new("Copy")
                    .icon(LucideIcon::Copy)
                    .variant(ButtonVariant::Ghost)
                    .size(ComponentSize::Sm),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Shortcut text").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Button::new("Save")
                    .variant(ButtonVariant::Outline)
                    .shortcut_text("Ctrl+S"),
            );
            _ = f.add(
                Button::new("Open")
                    .variant(ButtonVariant::Outline)
                    .shortcut_text("Ctrl+O"),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Selected (toggle)").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(
                Button::new("Toggle Me")
                    .variant(ButtonVariant::Outline)
                    .selected(self.demo.button_selected),
            )
            .inner
            .clicked()
            {
                self.demo.button_selected = !self.demo.button_selected;
            }
        });

        ui.add_space(12.0);
        _ = Typography::small("Disabled").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Disabled").enabled(false));
            _ = f.add(
                Button::new("Disabled Outline")
                    .variant(ButtonVariant::Outline)
                    .enabled(false),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Button group").show(ui);
        ui.add_space(4.0);
        _ = ButtonGroup::show(ui, |ui46| {
            _ = Button::new("Left")
                .variant(ButtonVariant::Outline)
                .show(ui46);
            _ = Button::new("Center")
                .variant(ButtonVariant::Outline)
                .show(ui46);
            _ = Button::new("Right")
                .variant(ButtonVariant::Outline)
                .show(ui46);
        });

        snippet(
            ui,
            "// Button: variants + sizes + icons\nuse functora_egui::{Button, ButtonGroup, ButtonVariant, ComponentSize, LucideIcon};\n\n// Variants\nButton::new(\"Default\").show(ui);\nButton::new(\"Destructive\").variant(ButtonVariant::Destructive).show(ui);\nButton::new(\"Outline\").variant(ButtonVariant::Outline).show(ui);\nButton::new(\"Ghost\").variant(ButtonVariant::Ghost).show(ui);\n\n// Sizes\nButton::new(\"XS\").size(ComponentSize::Xs).show(ui);\nButton::new(\"Small\").size(ComponentSize::Sm).show(ui);\nButton::new(\"Large\").size(ComponentSize::Lg).show(ui);\n\n// Icon + text\nButton::new(\"Download\").icon(LucideIcon::Download).show(ui);\nButton::new(\"Save\").shortcut_text(\"Ctrl+S\").variant(ButtonVariant::Outline).show(ui);\nButton::new(\"Open\").shortcut_text(\"Ctrl+O\").variant(ButtonVariant::Outline).show(ui);\n\n// Selected (toggle)\nButton::new(\"Toggle Me\").variant(ButtonVariant::Outline).selected(selected).show(ui);\n\n// Disabled\nButton::new(\"Disabled\").enabled(false).show(ui);\n\n// Button group\nButtonGroup::show(ui, |g| {\n    Button::new(\"Left\").variant(ButtonVariant::Outline).show(g);\n    Button::new(\"Center\").variant(ButtonVariant::Outline).show(g);\n    Button::new(\"Right\").variant(ButtonVariant::Outline).show(g);\n});",
        );
    }
}
