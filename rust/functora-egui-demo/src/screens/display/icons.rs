use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, LucideIcon, ScrollArea, Separator, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_icons(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("All 1600+ Lucide icons are available with one import.").show(ui);
        ui.add_space(12.0);
        _ = functora_egui::Input::new(&mut self.icon_search)
            .placeholder("Search icons...")
            .desired_width(260.0)
            .show(ui);
        ui.add_space(8.0);
        let needle = self.icon_search.trim().to_ascii_lowercase();
        let icons: Vec<LucideIcon> = functora_egui::icons::lucide_icon::ALL
            .iter()
            .copied()
            .filter(|icon| needle.is_empty() || icon.name().to_ascii_lowercase().contains(&needle))
            .collect();
        _ = Typography::small(format!("{} icons", icons.len())).show(ui);
        ui.add_space(6.0);
        _ = ScrollArea::new(320.0).show(ui, |ui21| {
            _ = ui21.horizontal_wrapped(|ui22| {
                for icon in icons {
                    if ui22
                        .add(
                            Button::icon_only(icon)
                                .variant(ButtonVariant::Ghost)
                                .size(functora_egui::ComponentSize::Sm),
                        )
                        .on_hover_text(icon.name())
                        .clicked()
                    {
                        self.toast.add(
                            icon.name(),
                            functora_egui::ToastVariant::Default,
                            ui22.ctx().input(|i| i.time),
                        );
                    }
                }
            });
        });
        ui.add_space(12.0);
        _ = Separator::horizontal().show(ui);
        ui.add_space(4.0);
        _ = Typography::small("Icons render from built-in SVG paths; no external font needed.")
            .show(ui);

        snippet(
            ui,
            "// Icons: 1600+ Lucide icons from built-in SVG paths\nuse functora_egui::{LucideIcon, Button, ButtonVariant, ComponentSize};\nuse functora_egui::icons::lucide_icon::ALL;\n\n// Search and display icons\nlet needle = \"settings\".to_ascii_lowercase();\nlet icons: Vec<LucideIcon> = ALL\n    .iter()\n    .copied()\n    .filter(|icon| icon.name().to_ascii_lowercase().contains(&needle))\n    .collect();\n\nfor icon in icons {\n    Button::icon_only(icon)\n        .variant(ButtonVariant::Ghost)\n        .size(ComponentSize::Sm)\n        .on_hover_text(icon.name())\n        .show(ui);\n}\n\n// Icons render from built-in SVG; no external font needed",
        );
    }
}
