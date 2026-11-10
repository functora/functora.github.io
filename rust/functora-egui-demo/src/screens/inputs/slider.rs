use functora_egui::snippet;
use functora_egui::{Slider, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_slider(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Drag to select a numeric value within a range.");
        _ = Slider::new(&mut self.slider_val, 0.0..=100.0)
            .step(1.0)
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Value: {:.0}", self.slider_val)).show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "With suffix");
        _ = Slider::new(&mut self.slider_price, 0.0..=1000.0)
            .step(10.0)
            .suffix(" USD")
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Budget: ${:.0}", self.slider_price)).show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "With prefix");
        _ = Slider::new(&mut self.slider_price, 0.0..=1000.0)
            .step(10.0)
            .prefix("$")
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Budget: ${:.0}", self.slider_price)).show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "f32 range");
        _ = Slider::f32(&mut self.slider_f32, 0.0..=1.0)
            .step(0.05)
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Level: {:.2}", self.slider_f32)).show(ui);

        snippet(
            ui,
            "// Slider: drag to select value in range\nuse functora_egui::Slider;\n\nlet mut value = 50.0;\nSlider::new(&mut value, 0.0..=100.0)\n    .step(1.0)\n    .width(400.0)\n    .show(ui);\n\n// With suffix\nlet mut price = 200.0;\nSlider::new(&mut price, 0.0..=1000.0)\n    .step(10.0)\n    .suffix(\" USD\")\n    .width(400.0)\n    .show(ui);\n\n// With prefix\nSlider::new(&mut price, 0.0..=1000.0)\n    .prefix(\"$\")\n    .show(ui);\n\n// f32 range\nlet mut level = 0.5f32;\nSlider::f32(&mut level, 0.0..=1.0)\n    .step(0.05)\n    .show(ui);",
        );
    }
}
