use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, HoverCard, Label, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
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
}
