use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_flex_wrap(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Flex rows wrap on narrow viewports; no_wrap_on_mobile keeps single-row toolbars.",
        )
        .show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Wrap (default)");
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for i in 0..8 {
                _ = f.add(Button::new(format!("Action {i}")).variant(ButtonVariant::Outline));
            }
        });
        ui.add_space(12.0);
        crate::screens::section(ui, "no_wrap_on_mobile: stays on one line");
        _ = Flex::row().gap(8.0).no_wrap_on_mobile().show(ui, |f| {
            for i in 0..4 {
                _ = f.add(Button::new(format!("Item {i}")).variant(ButtonVariant::Outline));
            }
        });

        snippet(
            ui,
            "// FlexWrap: rows wrap by default; no_wrap_on_mobile keeps toolbars on one line\nuse functora_egui::{Button, ButtonVariant, Flex};\n\nFlex::row().gap(8.0).wrap().show(ui, |f| {\n    for i in 0..8 {\n        f.add(Button::new(format!(\"Action {i}\")).variant(ButtonVariant::Outline));\n    }\n});\n\nFlex::row().gap(8.0).no_wrap_on_mobile().show(ui, |f| {\n    for i in 0..4 {\n        f.add(Button::new(format!(\"Item {i}\")).variant(ButtonVariant::Outline));\n    }\n});",
        );
    }
}
