use functora_egui::snippet;
use functora_egui::{Card, Flex, ResponsiveExt, Typography, TypographyVariant};

impl crate::state::ShowcaseApp {
    pub fn demo_spacing(ui: &mut egui::Ui) {
        _ = Typography::muted("Adaptive spacing scales touch targets and gaps on mobile.").show(ui);
        ui.add_space(12.0);
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui75| {
            _ = Flex::column().gap(8.0).align_start().show(ui75, |f| {
                for (name, value) in [
                    ("touch_height", format!("{:.1} px", spacing.touch_height)),
                    ("touch_padding", format!("{:.1} px", spacing.touch_padding)),
                    ("gap", format!("{:.1} px", spacing.gap)),
                    ("page_padding", format!("{:.1} px", spacing.page_padding)),
                    (
                        "content_max_width",
                        format!("{:.1} px", spacing.content_max_width),
                    ),
                ] {
                    _ = f.ui(|ui76| {
                        _ = ui76.horizontal(|ui77| {
                            _ = Typography::small(name)
                                .variant(TypographyVariant::Muted)
                                .show(ui77);
                            _ = ui77.label(value);
                        });
                    });
                }
            });
        });
        ui.add_space(8.0);
        if ui.on_mobile() {
            _ = Typography::small("Mobile spacing is active.").show(ui);
        } else {
            _ = Typography::small("Desktop spacing is active.").show(ui);
        }

        snippet(
            ui,
            "// Spacing: query responsive spacing and render it\nuse functora_egui::ResponsiveExt;\n\nlet spacing = ui.responsive_spacing();\n\neprintln!(\"touch_height: {:.1}px\", spacing.touch_height);\neprintln!(\"touch_padding: {:.1}px\", spacing.touch_padding);\neprintln!(\"gap: {:.1}px\", spacing.gap);\neprintln!(\"page_padding: {:.1}px\", spacing.page_padding);\neprintln!(\"content_max_width: {:.1}px\", spacing.content_max_width);",
        );
    }
}
