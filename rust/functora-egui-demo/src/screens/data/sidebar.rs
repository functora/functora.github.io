use functora_egui::Typography;
use functora_egui::snippet;

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_sidebar(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "The app sidebar is the navigation panel on the side. This page documents it, so there is no second live demo here.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(
            "On desktop it is a side panel, on mobile it slides in as a drawer toggled by the header hamburger button. Resize below 800px: the sidebar covers the screen as a drawer.",
        )
        .show(ui);

        snippet(
            ui,
            "// Sidebar: the app shell owns the only live sidebar, so this page is snippet-only\nuse functora_egui::{ResponsiveExt, Shell};\n\nlet mut collapsed = ui.on_mobile();\nlet mut selected = Some(\"Home\");\nShell::new(\"My app\", &mut collapsed, |side| {\n    for item in [\"Home\", \"Settings\", \"About\"] {\n        if side.selectable_label(Some(item) == selected, item).clicked() {\n            selected = Some(item);\n        }\n    }\n    false\n})\n.breadcrumb(route, history)\n.show(ui, |_content| {\n    // page content, no second Sidebar here\n});",
        );
    }
}
