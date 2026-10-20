use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, LucideIcon};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_alert_dialog(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "A confirmation dialog with a destructive action.");
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
}
