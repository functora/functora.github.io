use functora_egui::snippet;
use functora_egui::{Button, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_dialog(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A modal dialog with a backdrop.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("On mobile the dialog opens as a bottom sheet.").show(ui);
        ui.add_space(4.0);
        if Button::new("Open Dialog")
            .icon(LucideIcon::SquareMenu)
            .show(ui)
            .clicked()
        {
            self.dialogs.dialog_open = true;
        }
        ui.add_space(12.0);
        _ = Typography::small(
            "Shrink the window below 800px, then open the dialog: it slides up from the \
             bottom instead of centering.",
        )
        .show(ui);

        snippet(
            ui,
            "// Dialog: modal dialog with backdrop\n// On mobile Dialog anchors CENTER_BOTTOM as a bottom sheet.\nuse functora_egui::{Dialog, Button, ButtonVariant, LucideIcon, ComponentSize, Label, Input, Textarea, Flex};\n\nlet mut open = false;\n\nif Button::new(\"Open Dialog\").icon(LucideIcon::SquareMenu).show(ui).clicked() {\n    open = true;\n}\n\nDialog::new()\n    .title(\"Edit Profile\")\n    .description(\"Make changes to your profile here.\")\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Full name\").show(ui);\n        ui.add_space(8.0);\n        Input::new(&mut name).placeholder(\"Ada Lovelace\").show(ui);\n        ui.add_space(8.0);\n        Label::new(\"Bio\").show(ui);\n        ui.add_space(8.0);\n        Textarea::new(&mut bio).placeholder(\"Tell us about yourself...\").desired_width(ui.available_width()).show(ui);\n        ui.add_space(12.0);\n        Flex::row().justify_end().gap(8.0).show(ui, |f| {\n            f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline).size(ComponentSize::Sm));\n            if f.add(Button::new(\"Save Changes\").size(ComponentSize::Sm).icon(LucideIcon::Check)).clicked() {\n                open = false;\n            }\n        });\n    });",
        );
    }
}
