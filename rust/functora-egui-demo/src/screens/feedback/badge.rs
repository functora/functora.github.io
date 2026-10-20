use functora_egui::snippet;
use functora_egui::{Badge, BadgeVariant, Flex};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_badge(ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Small labels for counts, states, and statuses.");
        crate::screens::section(ui, "Variants");
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            _ = f.add(Badge::new("Default"));
            _ = f.add(Badge::new("Secondary").variant(BadgeVariant::Secondary));
            _ = f.add(Badge::new("Outline").variant(BadgeVariant::Outline));
            _ = f.add(Badge::new("Destructive").variant(BadgeVariant::Destructive));
        });
        ui.add_space(12.0);

        snippet(
            ui,
            "// Badge: small labels for counts, states, statuses\nuse functora_egui::{Badge, BadgeVariant, Flex};\n\nFlex::row().gap(8.0).wrap().show(ui, |f| {\n    f.add(Badge::new(\"Default\"));\n    f.add(Badge::new(\"Secondary\").variant(BadgeVariant::Secondary));\n    f.add(Badge::new(\"Outline\").variant(BadgeVariant::Outline));\n    f.add(Badge::new(\"Destructive\").variant(BadgeVariant::Destructive));\n});",
        );
    }
}
