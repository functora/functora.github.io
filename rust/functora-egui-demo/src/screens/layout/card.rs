use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, ComponentSize, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_card(ui: &mut egui::Ui) {
        _ = Typography::muted("Bordered container for grouping content.").show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui52| {
            _ = Typography::h4("Card Title").show(ui52);
            ui52.add_space(4.0);
            _ = ui52.label("This is a card with some descriptive content inside.");
            ui52.add_space(8.0);
            _ = Button::new("Action")
                .variant(ButtonVariant::Outline)
                .size(ComponentSize::Sm)
                .show(ui52);
        });
        ui.add_space(8.0);
        _ = Card::new().heading("Card Heading").show(ui, |ui53| {
            _ = ui53.label("Card with a built-in heading section.");
        });
        ui.add_space(8.0);

        snippet(
            ui,
            "// Card: bordered container for grouping content\nuse functora_egui::{Card, Button, ButtonVariant, ComponentSize};\n\nCard::new().show(ui, |card| {\n    card.add(Typography::h4(\"Card Title\"));\n    card.add_space(4.0);\n    card.label(\"This is a card with some descriptive content inside.\");\n    card.add_space(8.0);\n    Button::new(\"Action\")\n        .variant(ButtonVariant::Outline)\n        .size(ComponentSize::Sm)\n        .show(card);\n});\n\n// Card with a built-in heading\nCard::new()\n    .heading(\"Card Heading\")\n    .show(ui, |card| {\n        card.label(\"Card with a built-in heading section.\");\n    });",
        );
    }
}
