use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, LucideIcon, Tooltip, Typography};

impl crate::state::ShowcaseApp {
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
}
