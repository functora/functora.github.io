use functora_egui::snippet;
use functora_egui::{Alert, AlertVariant, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_alert(ui: &mut egui::Ui) {
        _ = Typography::muted("A status message container with variants.").show(ui);
        ui.add_space(12.0);
        _ = Alert::new()
            .title("Heads up!")
            .variant(AlertVariant::Default)
            .show(ui, |ui23| {
                _ = ui23.label("You can add components to your app using the CLI.");
            });
        ui.add_space(8.0);
        _ = Alert::new()
            .title("Error")
            .variant(AlertVariant::Destructive)
            .show(ui, |ui24| {
                _ = ui24.label("Your session has expired. Please log in again.");
            });
        ui.add_space(8.0);
        _ = Alert::new()
            .title("Success")
            .variant(AlertVariant::Success)
            .show(ui, |ui25| {
                _ = ui25.label("Your changes have been saved.");
            });
        ui.add_space(8.0);
        _ = Alert::new()
            .title("Warning")
            .variant(AlertVariant::Warning)
            .show(ui, |ui26| {
                _ = ui26.label("Your account will expire soon.");
            });
        ui.add_space(8.0);
        _ = Alert::new()
            .title("Info")
            .variant(AlertVariant::Info)
            .show(ui, |ui27| {
                _ = ui27.label("A new version is available.");
            });

        snippet(
            ui,
            "// Alert: styled alert messages\nuse functora_egui::{Alert, AlertVariant};\n\nAlert::new()\n    .title(\"Heads up!\")\n    .variant(AlertVariant::Default)\n    .show(ui, |ui| {\n        ui.label(\"You can add components to your app using the CLI.\");\n    });\n\nAlert::new()\n    .title(\"Error\")\n    .variant(AlertVariant::Destructive)\n    .show(ui, |ui| {\n        ui.label(\"Your session has expired. Please log in again.\");\n    });\n\nAlert::new()\n    .title(\"Success\")\n    .variant(AlertVariant::Success)\n    .show(ui, |ui| {\n        ui.label(\"Your changes have been saved.\");\n    });\n\nAlert::new()\n    .title(\"Warning\")\n    .variant(AlertVariant::Warning)\n    .show(ui, |ui| {\n        ui.label(\"Your account will expire soon.\");\n    });\n\nAlert::new()\n    .title(\"Info\")\n    .variant(AlertVariant::Info)\n    .show(ui, |ui| {\n        ui.label(\"A new version is available.\");\n    });",
        );
    }
}
