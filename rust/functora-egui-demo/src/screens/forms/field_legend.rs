use functora_egui::snippet;
use functora_egui::{FieldDescription, FieldLegend, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_field_legend(ui: &mut egui::Ui) {
        _ = Typography::muted("A legend heading for a field group.").show(ui);
        ui.add_space(12.0);
        FieldLegend::show(ui, "Payment details");
        ui.add_space(4.0);
        FieldDescription::show(ui, "All transactions are secure and encrypted.");
        ui.add_space(8.0);
        FieldLegend::show(ui, "Billing address");
        ui.add_space(4.0);
        FieldDescription::show(ui, "Used only for invoices and receipts.");

        snippet(
            ui,
            "// FieldLegend + FieldDescription\nuse functora_egui::{FieldLegend, FieldDescription};\n\nFieldLegend::show(ui, \"Payment details\");\nFieldDescription::show(ui, \"All transactions are secure and encrypted.\");\n\nFieldLegend::show(ui, \"Billing address\");\nFieldDescription::show(ui, \"Used only for invoices and receipts.\");",
        );
    }
}
