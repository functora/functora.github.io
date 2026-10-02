use functora_egui::snippet;
use functora_egui::{Slider, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_slider(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Drag to select a numeric value within a range.").show(ui);
        ui.add_space(12.0);
        _ = Slider::new(&mut self.slider_val, 0.0..=100.0)
            .step(1.0)
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Value: {:.0}", self.slider_val)).show(ui);

        ui.add_space(12.0);
        _ = Typography::small("With suffix").show(ui);
        ui.add_space(4.0);
        _ = Slider::new(&mut self.slider_price, 0.0..=1000.0)
            .step(10.0)
            .suffix(" USD")
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Budget: ${:.0}", self.slider_price)).show(ui);

        snippet(
            ui,
            "// Slider: drag to select value in range\nuse functora_egui::Slider;\n\nlet mut value = 50.0;\nSlider::new(&mut value, 0.0..=100.0)\n    .step(1.0)\n    .width(400.0)\n    .show(ui);\n\n// With suffix\nlet mut price = 200.0;\nSlider::new(&mut price, 0.0..=1000.0)\n    .step(10.0)\n    .suffix(\" USD\")\n    .width(400.0)\n    .show(ui);",
        );
    }
}
