use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Label, LucideIcon, Popover, Typography};

impl crate::state::ShowcaseApp {
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
}
