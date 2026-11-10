use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, LucideIcon, ScrollArea, Separator, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_icons(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "All 1600+ Lucide icons are available with one import.");
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Icon geometry");
        let elements =
            functora_egui::icons::parse_svg::parse_svg(LucideIcon::CircleUser.svg_data());
        _ = Typography::small(format!(
            "CircleUser parses into {} SVG elements: {:?}",
            elements.len(),
            elements
                .iter()
                .map(|element| match element {
                    functora_egui::icons::icon_element::IconElement::Path(commands) => {
                        format!("Path({})", commands.len())
                    }
                    functora_egui::icons::icon_element::IconElement::Circle { .. } => {
                        "Circle".to_owned()
                    }
                    functora_egui::icons::icon_element::IconElement::Rect { .. } => {
                        "Rect".to_owned()
                    }
                    functora_egui::icons::icon_element::IconElement::Line { .. } => {
                        "Line".to_owned()
                    }
                    functora_egui::icons::icon_element::IconElement::Polyline(_) => {
                        "Polyline".to_owned()
                    }
                    functora_egui::icons::icon_element::IconElement::Polygon(_) => {
                        "Polygon".to_owned()
                    }
                    functora_egui::icons::icon_element::IconElement::Ellipse { .. } => {
                        "Ellipse".to_owned()
                    }
                })
                .collect::<Vec<_>>()
        ))
        .show(ui);
        let commands = functora_egui::icons::parse_path::parse_path_data("M0 0 L10 10 Z");
        _ = Typography::small(format!(
            "parse_path_data(\"M0 0 L10 10 Z\") -> {} commands, first: {:?}",
            commands.len(),
            commands.first()
        ))
        .show(ui);
        _ = Typography::small(format!(
            "LucideIcon::CircleUser.svg_data(): {:?}",
            LucideIcon::CircleUser.svg_data()
        ))
        .show(ui);
        _ = Typography::small(
            "The generated SVG bodies live in functora_egui::icons::lucide_icon_data.",
        )
        .show(ui);
        ui.add_space(4.0);
        let (rect, _) = ui.allocate_exact_size(egui::vec2(32.0, 32.0), egui::Sense::hover());
        functora_egui::icons::paint_icon::paint_icon_svg(
            &ui.painter_at(rect),
            rect,
            "<circle cx=\"12\" cy=\"12\" r=\"10\" />",
            ui.visuals().text_color(),
        );
        _ = Typography::small("paint_icon_svg draws a custom Lucide-style SVG body.").show(ui);

        snippet(
            ui,
            "// Icons: 1600+ Lucide icons from built-in SVG paths\nuse functora_egui::{LucideIcon, Button, ButtonVariant, ComponentSize};\nuse functora_egui::icons::lucide_icon::ALL;\n\n// Search and display icons\nlet needle = \"settings\".to_ascii_lowercase();\nlet icons: Vec<LucideIcon> = ALL\n    .iter()\n    .copied()\n    .filter(|icon| icon.name().to_ascii_lowercase().contains(&needle))\n    .collect();\n\nfor icon in icons {\n    Button::icon_only(icon)\n        .variant(ButtonVariant::Ghost)\n        .size(ComponentSize::Sm)\n        .on_hover_text(icon.name())\n        .show(ui);\n}\n\n// Icons render from built-in SVG; no external font needed\n\n// Icon geometry: parse an icon into SVG elements and path commands\nuse functora_egui::icons::icon_element::IconElement;\nuse functora_egui::icons::parse_path::parse_path_data;\nuse functora_egui::icons::parse_svg::parse_svg;\n\nlet elements = parse_svg(LucideIcon::CircleUser.svg_data());\nassert!(!elements.is_empty());\n\nlet commands = parse_path_data(\"M0 0 L10 10 Z\");\nassert_eq!(commands.len(), 3);\n\n// Every command is a typed variant\nuse functora_egui::icons::path_command::PathCommand;\n\nassert!(matches!(commands[0], PathCommand::MoveToAbs(0.0, 0.0)));\nassert!(matches!(commands[2], PathCommand::Close));\n\n// The generated SVG bodies live in their own module\nuse functora_egui::icons::lucide_icon_data;\n\nlet svg = LucideIcon::CircleUser.svg_data();\nassert!(!svg.is_empty());\n\n// Paint a custom icon that is not part of the Lucide set\nuse functora_egui::icons::paint_icon::paint_icon_svg;\n\nlet (rect, _) = ui.allocate_exact_size(egui::vec2(32.0, 32.0), egui::Sense::hover());\npaint_icon_svg(\n    &ui.painter_at(rect),\n    rect,\n    \"<circle cx=\\\"12\\\" cy=\\\"12\\\" r=\\\"10\\\" />\",\n    ui.visuals().text_color(),\n);",
        );
    }
}
