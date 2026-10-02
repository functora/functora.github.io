use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Empty, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_empty(ui: &mut egui::Ui) {
        _ = Typography::muted("A centered empty state for lists and searches.").show(ui);
        ui.add_space(12.0);
        _ = Empty::show(ui, |ui25| {
            _ = Card::new().show(ui25, |ui26| {
                _ = Button::icon_only(LucideIcon::Inbox)
                    .variant(ButtonVariant::Ghost)
                    .size(functora_egui::ComponentSize::Lg)
                    .show(ui26);
                ui26.add_space(4.0);
                _ = Typography::h4("No results found").show(ui26);
                ui26.add_space(4.0);
                _ = Typography::small("Try adjusting your search to find what you're looking for.")
                    .show(ui26);
                ui26.add_space(8.0);
                _ = Button::new("Reset Search")
                    .variant(ButtonVariant::Outline)
                    .size(functora_egui::ComponentSize::Sm)
                    .show(ui26);
            });
        });

        snippet(
            ui,
            "// Empty: centered empty state for lists/searches\nuse functora_egui::{Empty, Card, Button, ButtonVariant, LucideIcon, Typography, ComponentSize};\n\nEmpty::show(ui, |ui| {\n    Card::new().show(ui, |card| {\n        Button::icon_only(LucideIcon::Inbox)\n            .variant(ButtonVariant::Ghost)\n            .size(ComponentSize::Lg)\n            .show(card);\n        card.add_space(4.0);\n        Typography::h4(\"No results found\").show(card);\n        card.add_space(4.0);\n        Typography::small(\"Try adjusting your search to find what you're looking for.\").show(card);\n        card.add_space(8.0);\n        Button::new(\"Reset Search\")\n            .variant(ButtonVariant::Outline)\n            .size(ComponentSize::Sm)\n            .show(card);\n    });\n});",
        );
    }
}
