use functora_egui::snippet;
use functora_egui::{Card, Flex, ResponsiveExt, Typography, TypographyVariant};

impl crate::state::ShowcaseApp {
    pub fn demo_breakpoint(ui: &mut egui::Ui) {
        _ = Typography::muted("The viewport breakpoint switches at 800px: mobile vs desktop.")
            .show(ui);
        ui.add_space(12.0);
        let bp = ui.breakpoint();
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui71| {
            _ = Flex::column().gap(8.0).align_start().show(ui71, |f| {
                _ = f.ui(|ui72| {
                    _ = Typography::small(format!("Breakpoint: {bp:?}")).show(ui72);
                });
                _ = f.ui(|ui73| {
                    _ = Typography::small(if bp.is_mobile() { "mobile" } else { "desktop" })
                        .variant(TypographyVariant::Muted)
                        .show(ui73);
                });
                _ = f.ui(|ui74| {
                    _ = Typography::small(format!("Spacing: {spacing:?}")).show(ui74);
                });
            });
        });
        ui.add_space(12.0);
        _ = Typography::small("Resize the window below 800px to flip the breakpoint.").show(ui);

        snippet(
            ui,
            "// Breakpoint: mobile vs desktop detection\nuse functora_egui::{Breakpoint, ResponsiveExt, Spacing};\n\nlet bp = ui.breakpoint();\nlet spacing = ui.responsive_spacing();\n\nif bp.is_mobile() {\n    // Compact layout\n} else {\n    // Full layout\n}\n\n// spacing: Spacing { touch_height, touch_padding, gap, page_padding, content_max_width }",
        );
    }
}
