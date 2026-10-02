use functora_egui::snippet;
use functora_egui::{
    Badge, Button, ButtonVariant, Flex, FlexAlign, FlexItem, Input, Label, Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_flex(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Flexbox-like layout with gap, grow, justify, align, wrap, and spacer.",
        )
        .show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Row with gap").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Cancel").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Save"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Column with gap").show(ui);
        ui.add_space(4.0);
        _ = Flex::column().gap(8.0).align_start().show(ui, |f| {
            _ = f.add(Badge::new("First"));
            _ = f.add(Badge::new("Second"));
            _ = f.add(Badge::new("Third"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Grow: input fills, button stays natural").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).w_full().show(ui, |f| {
            _ = f.grow(
                1.0,
                Input::new(&mut self.flex_input).placeholder("Type a message..."),
            );
            _ = f.add(Button::new("Send"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Justify end").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().justify_end().gap(8.0).w_full().show(ui, |f| {
            _ = f.add(Button::new("Cancel").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Confirm"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Justify between").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().justify_between().w_full().show(ui, |f| {
            _ = f.add(Button::new("Previous").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Next"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Justify center").show(ui);
        ui.add_space(4.0);
        _ = Flex::row()
            .justify_center()
            .gap(8.0)
            .w_full()
            .show(ui, |f| {
                _ = f.add(functora_egui::Spinner::new().size(20.0));
                _ = f.ui(|ui69| {
                    _ = ui69.label("Loading...");
                });
            });

        ui.add_space(12.0);
        _ = Typography::small("Wrap: overflowing items wrap to the next line").show(ui);
        ui.add_space(4.0);
        let tags = [
            "Rust",
            "egui",
            "shadcn",
            "flexbox",
            "layout",
            "widgets",
            "responsive",
            "wrap",
            "gap",
            "grow",
            "theming",
            "buttons",
            "inputs",
            "cards",
            "dialogs",
            "toasts",
            "badges",
        ];
        _ = Flex::row().gap(4.0).wrap().w_full().show(ui, |f| {
            for tag in tags {
                _ = f.add(Badge::new(tag));
            }
        });

        ui.add_space(12.0);
        _ = Typography::small("Spacer: pushes items apart").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).w_full().show(ui, |f| {
            _ = f.add(Badge::new("Left"));
            _ = f.spacer();
            _ = f.add(Badge::new("Right"));
        });

        ui.add_space(12.0);
        _ = Typography::small("Nested flex: two-column form").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(16.0).w_full().show(ui, |f| {
            _ = f.grow_nested(1.0, Flex::column().gap(8.0), |f4| {
                _ = f4.item_ui(FlexItem::new().align_self(FlexAlign::Start), |ui70| {
                    _ = Label::new("First Name").show(ui70)
                });
                _ = f4.add(Input::new(&mut self.flex_first).placeholder("John"));
                _ = f4.item_ui(FlexItem::new().align_self(FlexAlign::Start), |ui71| {
                    _ = Label::new("Last Name").show(ui71)
                });
                _ = f4.add(Input::new(&mut self.flex_last).placeholder("Doe"));
            });
            _ = f.grow_nested(1.0, Flex::column().gap(8.0), |f5| {
                _ = f5.item_ui(FlexItem::new().align_self(FlexAlign::Start), |ui72| {
                    _ = Label::new("Email").show(ui72)
                });
                _ = f5.add(Input::new(&mut self.flex_email).placeholder("john@example.com"));
                _ = f5.item_ui(FlexItem::new().align_self(FlexAlign::Start), |ui73| {
                    _ = Label::new("Phone").show(ui73)
                });
                _ = f5.add(Input::new(&mut self.flex_phone).placeholder("+1 555-1234"));
            });
        });

        ui.add_space(12.0);
        _ = Typography::small("Center utility").show(ui);
        ui.add_space(4.0);
        _ = egui::Frame::NONE
            .stroke(egui::Stroke::new(1.0, egui::Color32::from_gray(80)))
            .show(ui, |ui47| {
                ui47.set_min_height(80.0);
                _ = functora_egui::center(ui47, |ui48| {
                    _ = ui48.horizontal(|ui49| {
                        _ = functora_egui::Spinner::new().size(16.0).show(ui49);
                        ui49.add_space(8.0);
                        _ = ui49.label("Centered content");
                    });
                });
            });

        snippet(
            ui,
            "// Flex: flexbox-like layout with gap, grow, justify, align, wrap\nuse functora_egui::Flex;\n\n// Row with gap\nFlex::row().gap(8.0).show(ui, |f| {\n    f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline));\n    f.add(Button::new(\"Save\"));\n});\n\n// Column with gap\nFlex::column().gap(8.0).align_start().show(ui, |f| {\n    f.add(Badge::new(\"First\"));\n    f.add(Badge::new(\"Second\"));\n    f.add(Badge::new(\"Third\"));\n});\n\n// Grow: input fills, button stays natural\nFlex::row().gap(8.0).w_full().show(ui, |f| {\n    f.grow(1.0, Input::new(&mut text).placeholder(\"Type a message...\"));\n    f.add(Button::new(\"Send\"));\n});\n\n// Justify end\nFlex::row().justify_end().gap(8.0).w_full().show(ui, |f| {\n    f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline));\n    f.add(Button::new(\"Confirm\"));\n});\n\n// Justify between\nFlex::row().justify_between().w_full().show(ui, |f| {\n    f.add(Button::new(\"Previous\").variant(ButtonVariant::Outline));\n    f.add(Button::new(\"Next\"));\n});\n\n// Justify center\nFlex::row().justify_center().gap(8.0).w_full().show(ui, |f| {\n    f.add(Spinner::new().size(20.0));\n    f.ui(|ui| { ui.label(\"Loading...\"); });\n});\n\n// Spacer pushes items apart\nFlex::row().gap(8.0).w_full().show(ui, |f| {\n    f.add(Badge::new(\"Left\"));\n    f.spacer();\n    f.add(Badge::new(\"Right\"));\n});\n\n// Nested flex: two-column form\nFlex::row().gap(16.0).w_full().show(ui, |f| {\n    f.grow_nested(1.0, Flex::column().gap(8.0), |col| {\n        col.add(Input::new(&mut first).placeholder(\"John\"));\n        col.add(Input::new(&mut last).placeholder(\"Doe\"));\n    });\n    f.grow_nested(1.0, Flex::column().gap(8.0), |col| {\n        col.add(Input::new(&mut email).placeholder(\"john@example.com\"));\n    });\n});\n\n// Center utility\ncenter(ui, |ui| {\n    ui.label(\"Centered content\");\n});\n\n// Wrap\nFlex::row().gap(4.0).wrap().w_full().show(ui, |f| {\n    for tag in [\"Rust\", \"egui\", \"shadcn\", \"flexbox\", \"layout\", \"widgets\", \"responsive\", \"wrap\", \"gap\", \"grow\", \"theming\", \"buttons\", \"inputs\", \"cards\", \"dialogs\", \"toasts\", \"badges\"] {\n        f.add(Badge::new(tag));\n    }\n});",
        );
    }
}
