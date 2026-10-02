use functora_egui::snippet;
use functora_egui::{ScrollArea, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_scroll_area(ui: &mut egui::Ui) {
        _ = Typography::muted("Themed scrollable region with max height.").show(ui);
        ui.add_space(12.0);
        _ = ScrollArea::new(360.0).show(ui, |ui60| {
            for i in 1..=40 {
                _ = ui60.label(format!("Scrollable item {i} — long content to demonstrate scrolling and make area bigger"));
            }
        });

        snippet(
            ui,
            "// ScrollArea: themed scrollable region\nuse functora_egui::ScrollArea;\n\nScrollArea::new(360.0).show(ui, |scroll| {\n    for i in 1..=40 {\n        scroll.label(format!(\"Scrollable item {i} — long content to demonstrate scrolling and make area bigger\"));\n    }\n});",
        );
    }
}
