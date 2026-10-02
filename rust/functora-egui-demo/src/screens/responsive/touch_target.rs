use functora_egui::snippet;
use functora_egui::{Button, Card, Flex, ResponsiveExt, Slider, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_touch_target(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Controls use touch-friendly heights and padding on mobile.")
            .show(ui);
        ui.add_space(12.0);
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui78| {
            _ = Flex::column().gap(8.0).align_start().show(ui78, |f| {
                _ = f.ui(|ui79| {
                    _ = Typography::small(format!(
                        "Touch target height: {:.0} px (desktop 36, mobile 48)",
                        spacing.touch_height
                    ))
                    .show(ui79);
                });
                _ = f.ui(|ui80| {
                    _ = Typography::small(format!(
                        "Touch padding: {:.1} px",
                        spacing.touch_padding
                    ))
                    .show(ui80);
                });
            });
        });
        ui.add_space(12.0);
        _ = Typography::small("Default button with responsive spacing").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Touch me"));
        });
        ui.add_space(12.0);
        _ = Typography::small("Slider with responsive height").show(ui);
        ui.add_space(4.0);
        _ = Slider::new(&mut self.touch_slider_val, 0.0..=100.0)
            .step(1.0)
            .width(ui.available_width().min(360.0))
            .show(ui);

        snippet(
            ui,
            "// Touch target: responsive heights and padding\nuse functora_egui::{ResponsiveExt, Button, Slider, Flex, Card};\n\nlet spacing = ui.responsive_spacing();\n\n// touch_height: 36px desktop, 48px mobile\n// touch_padding: extra padding for touch\n// Button and Slider automatically use these\n\nFlex::row().gap(8.0).show(ui, |f| {\n    f.add(Button::new(\"Touch me\"));\n});\n\nSlider::new(&mut val, 0.0..=100.0)\n    .step(1.0)\n    .width(360.0)\n    .show(ui);",
        );
    }
}
