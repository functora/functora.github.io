//! Responsive: breakpoints, adaptive spacing, flex wrapping, touch targets,
//! and mobile-first behavior demos.

use functora_egui::{
    Button, ButtonVariant, Card, Flex, ResponsiveExt, Slider, Typography, TypographyVariant,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_breakpoint(ui: &mut egui::Ui) {
        _ = Typography::muted("The viewport breakpoint switches at 800px: mobile vs desktop.")
            .show(ui);
        ui.add_space(12.0);
        let bp = ui.breakpoint();
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui71| {
            _ = Flex::column().gap(8.0).show(ui71, |f| {
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

    pub(crate) fn demo_spacing(ui: &mut egui::Ui) {
        _ = Typography::muted("Adaptive spacing scales touch targets and gaps on mobile.").show(ui);
        ui.add_space(12.0);
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui75| {
            _ = Flex::column().gap(8.0).show(ui75, |f| {
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
            "// Touch target: responsive heights and padding\\nuse functora_egui::{ResponsiveExt, Button, Slider, Flex, Card};\\n\\nlet spacing = ui.responsive_spacing();\\n\\n// touch_height: 36px desktop, 48px mobile\\n// touch_padding: extra padding for touch\\n// Button and Slider automatically use these\\n\\nFlex::row().gap(8.0).show(ui, |f| {\\n    f.add(Button::new(\"Touch me\"));\\n});\\n\\nSlider::new(&mut val, 0.0..=100.0)\\n    .step(1.0)\\n    .width(360.0)\\n    .show(ui);",
        );
    }

    pub(crate) fn demo_flex_wrap(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Flex rows wrap on narrow viewports; no_wrap_on_mobile keeps single-row toolbars.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Wrap (default)").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for i in 0..8 {
                _ = f.add(Button::new(format!("Action {i}")).variant(ButtonVariant::Outline));
            }
        });
        ui.add_space(12.0);
        _ = Typography::small("no_wrap_on_mobile: stays on one line").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).no_wrap_on_mobile().show(ui, |f| {
            for i in 0..4 {
                _ = f.add(Button::new(format!("Item {i}")).variant(ButtonVariant::Outline));
            }
        });

        snippet(
            ui,
            "Flex::row().gap(8.0).wrap().show(ui, |f| { ... });\nFlex::row().no_wrap_on_mobile().show(ui, |f| { ... });",
        );

        snippet(
            ui,
            "Flex::row().gap(8.0).wrap().show(ui, |f| { ... });\nFlex::row().no_wrap_on_mobile().show(ui, |f| { ... });",
        );
    }

    pub(crate) fn demo_touch_target(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Controls use touch-friendly heights and padding on mobile.")
            .show(ui);
        ui.add_space(12.0);
        let spacing = ui.responsive_spacing();
        _ = Card::new().show(ui, |ui78| {
            _ = Flex::column().gap(8.0).show(ui78, |f| {
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
        _ = Slider::new(&mut self.slider_val, 0.0..=100.0)
            .step(1.0)
            .width(ui.available_width().min(360.0))
            .show(ui);

        snippet(
            ui,
            "// Touch target: responsive heights and padding\\nuse functora_egui::{ResponsiveExt, Button, Slider, Flex, Card};\\n\\nlet spacing = ui.responsive_spacing();\\n\\n// touch_height: 36px desktop, 48px mobile\\n// touch_padding: extra padding for touch\\n// Button and Slider automatically use these\\n\\nFlex::row().gap(8.0).show(ui, |f| {\\n    f.add(Button::new(\"Touch me\"));\\n});\\n\\nSlider::new(&mut val, 0.0..=100.0)\\n    .step(1.0)\\n    .width(360.0)\\n    .show(ui);",
        );
    }

    pub(crate) fn demo_mobile_dialog(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Dialogs become bottom sheets on mobile viewports.").show(ui);
        ui.add_space(12.0);
        if Button::new("Open Dialog")
            .icon(functora_egui::LucideIcon::Smartphone)
            .variant(ButtonVariant::Outline)
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
    }

    pub(crate) fn demo_mobile_sidebar(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "The app sidebar slides in as a drawer overlay on mobile. This page documents it, so there is no second live demo here.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(
            "Resize below 800px: the sidebar covers the screen as a drawer. The header \
             hamburger button toggles it.",
        )
        .show(ui);

        snippet(
            ui,
            "// MobileSidebar: Shell renders the same sidebar as a drawer on mobile\n// No second Sidebar is rendered inside page content.\nuse functora_egui::{ResponsiveExt, Shell};\n\nlet mut collapsed = ui.on_mobile();\nShell::new(\"functora-egui\", &mut collapsed, |side| {\n    // Same CATEGORIES loop as the Sidebar page.\n    false\n})\n.show(ui, |content| {\n    // page content, drawer is owned by Shell\n});",
        );
    }
}
