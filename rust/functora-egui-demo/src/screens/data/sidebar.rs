use functora_egui::ResponsiveExt;
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Sidebar builder options");
        _ = Typography::small("Sidebar::new() - 256 px, responsive drawer").show(ui);
        _ = Typography::small("Sidebar::width(320.0) - fixed panel width").show(ui);
        _ = Typography::small("Sidebar::collapsible() - renders the toggle button").show(ui);
        _ = Typography::small("Sidebar::static_() - never collapses into a drawer").show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Toggle button and effective width");
        let mut collapsed = ui.on_mobile();
        _ = Typography::small(
            "Sidebar::toggle_button renders the hamburger that opens the mobile drawer.",
        )
        .show(ui);
        _ = functora_egui::Sidebar::toggle_button(ui, &mut collapsed);
        _ = Typography::small(format!(
            "sidebar_effective_width(ctx, labels): {:.1}",
            functora_egui::sidebar_effective_width(ui.ctx(), &["Home", "Settings", "About"])
        ))
        .show(ui);

        snippet(
            ui,
            "// Sidebar: the app shell owns the only live sidebar, so this page is snippet-only\nuse functora_egui::{ResponsiveExt, Shell, Sidebar};\n\nlet mut collapsed = ui.on_mobile();\nlet mut selected = Some(\"Home\");\nShell::new(\"My app\", &mut collapsed, |side| {\n    for item in [\"Home\", \"Settings\", \"About\"] {\n        if side.selectable_label(Some(item) == selected, item).clicked() {\n            selected = Some(item);\n        }\n    }\n    false\n})\n.breadcrumb(route, history)\n.show(ui, |_content| {\n    // page content, no second Sidebar here\n});\n\n// Standalone sidebar: 320 px wide, collapsible, and always inline (never a drawer)\nSidebar::new()\n    .width(320.0)\n    .collapsible()\n    .static_()\n    .show(ui, &mut collapsed, |side| {\n        side.selectable_label(selected == Some(\"Home\"), \"Home\");\n    });\n\n// The hamburger that opens the mobile drawer lives outside the sidebar\nSidebar::toggle_button(ui, &mut collapsed);\n\n// The width the sidebar needs for its longest label\nuse functora_egui::sidebar_effective_width;\n\nlet width = sidebar_effective_width(ctx, &[\"Home\", \"Settings\", \"About\"]);",
        );
    }
}
