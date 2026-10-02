use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_toast(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Transient notifications with variants and descriptions.").show(ui);
        ui.add_space(12.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Default").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.toast.add(
                    "Default toast",
                    functora_egui::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Success").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.toast.add(
                    "Success toast",
                    functora_egui::ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Destructive").variant(ButtonVariant::Destructive))
                .inner
                .clicked()
            {
                self.toast.add(
                    "Destructive toast",
                    functora_egui::ToastVariant::Error,
                    ctx.input(|i| i.time),
                );
            }
        });
        ui.add_space(12.0);
        if Button::new("Toast with description")
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.toast.add_with_description(
                "Scheduled: Catch up",
                "Friday, February 10, 2026 at 5:57 PM",
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(8.0);
        if Button::new("Long multiline toast")
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.toast.add_with_description(
                "Sync completed with a very long multiline title that wraps across several lines",
                "Uploaded 128 files, skipped 3 files, and downloaded 42 files in the background. Next sync is scheduled automatically when the device is back online.",
                functora_egui::ToastVariant::Success,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// Toast: transient notifications\nuse functora_egui::{ToastState, ToastVariant, Button, ButtonVariant, Flex};\n\nlet mut toast = ToastState::new();\nlet ctx = ui.ctx();\n\nFlex::row().gap(8.0).wrap().show(ui, |f| {\n    if f.add(Button::new(\"Default\").variant(ButtonVariant::Outline)).inner.clicked() {\n        toast.add(\"Default toast\", ToastVariant::Default, ctx.input(|i| i.time));\n    }\n    if f.add(Button::new(\"Success\").variant(ButtonVariant::Outline)).inner.clicked() {\n        toast.add(\"Success toast\", ToastVariant::Success, ctx.input(|i| i.time));\n    }\n    if f.add(Button::new(\"Destructive\").variant(ButtonVariant::Destructive)).inner.clicked() {\n        toast.add(\"Destructive toast\", ToastVariant::Error, ctx.input(|i| i.time));\n    }\n});\n\n// With description\ntoast.add_with_description(\n    \"Scheduled: Catch up\",\n    \"Friday, February 10, 2026 at 5:57 PM\",\n    ToastVariant::Default,\n    ctx.input(|i| i.time),\n);\n\n// Long multiline toast\ntoast.add_with_description(\n    \"Sync completed with a very long multiline title that wraps across several lines\",\n    \"Uploaded 128 files, skipped 3 files, and downloaded 42 files in the background.\",\n    ToastVariant::Success,\n    ctx.input(|i| i.time),\n);\n\n// Call toast.show(&ctx) in your render loop",
        );
    }
}
