use functora_egui::snippet;
use functora_egui::{Flex, NumberInput, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_number_input(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Numeric input with drag, range, prefix/suffix.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("f64 with range and suffix").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::new(&mut self.number_f64)
                    .range(0.0..=100.0)
                    .speed(0.5)
                    .suffix("px")
                    .width(110.0),
            );
            _ = f.ui(|ui65| {
                _ = Typography::small(format!("{:.1}", self.number_f64)).show(ui65);
            });
        });

        ui.add_space(12.0);
        _ = Typography::small("f32 with decimals and prefix").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::f32(&mut self.number_f32)
                    .decimals(2)
                    .prefix("$")
                    .width(90.0),
            );
            _ = f.ui(|ui66| {
                _ = Typography::small(format!("{:.2}", self.number_f32)).show(ui66);
            });
        });

        ui.add_space(12.0);
        _ = Typography::small("i32 integer").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::i32(&mut self.number_i32)
                    .range(0.0..=50.0)
                    .width(70.0),
            );
            _ = f.ui(|ui67| {
                _ = Typography::small(format!("{}", self.number_i32)).show(ui67);
            });
        });

        snippet(
            ui,
            "// NumberInput: numeric input with drag, range, prefix/suffix\nuse functora_egui::NumberInput;\n\n// f64 with range and suffix\nlet mut px = 0.0;\nNumberInput::new(&mut px)\n    .range(0.0..=100.0)\n    .speed(0.5)\n    .suffix(\"px\")\n    .width(110.0)\n    .show(ui);\n\n// f32 with decimals and prefix\nlet mut price = 0.0_f32;\nNumberInput::f32(&mut price)\n    .decimals(2)\n    .prefix(\"$\")\n    .width(90.0)\n    .show(ui);\n\n// i32 integer\nlet mut count = 0i32;\nNumberInput::i32(&mut count)\n    .range(0.0..=50.0)\n    .width(70.0)\n    .show(ui);",
        );
    }
}
