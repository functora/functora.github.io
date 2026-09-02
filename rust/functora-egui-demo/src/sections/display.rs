//! Display: typography, labels, keyboard hints, items, and the icon catalog.

use functora_egui::{
    Button, ButtonVariant, Flex, Item, Kbd, Label, LucideIcon, ScrollArea, Separator, Typography,
    TypographyVariant,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_typography(ui: &mut egui::Ui) {
        _ = Typography::muted("Text styles: headings, lead, muted, and small.").show(ui);
        ui.add_space(12.0);
        _ = Typography::h1("The Joke Tax Chronicles").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "Once upon a time, in a far-off land, there was a very lazy king who spent all day \
             lounging on his throne. One day, his advisors came to him with a problem.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h2("The King's Plan").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "The king thought long and hard, and finally came up with a brilliant plan.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h3("The Joke").show(ui);
        ui.add_space(4.0);
        _ = Typography::new("Why did the chicken cross the road? To get to the other side.")
            .show(ui);
        ui.add_space(8.0);
        _ = Typography::h4("People stopped telling jokes").show(ui);
        ui.add_space(4.0);
        _ = Typography::small("The moral of the story is: this is a typography demo.").show(ui);
        ui.add_space(8.0);
        _ = Typography::lead("This is a lead paragraph: slightly larger and muted.").show(ui);
        ui.add_space(8.0);
        _ = Typography::muted("Muted text is dimmer for secondary content.").show(ui);
        ui.add_space(8.0);
        _ = Typography::new("Plain paragraph style with a custom variant.")
            .variant(TypographyVariant::Large)
            .show(ui);

        snippet(
            ui,
            "// Typography: styled text with variants\nuse functora_egui::{Typography, TypographyVariant};\n\nTypography::h1(\"Title\").show(ui);\nTypography::h2(\"Subtitle\").show(ui);\nTypography::h3(\"Heading\").show(ui);\nTypography::h4(\"Sub-heading\").show(ui);\nTypography::small(\"fine print\").show(ui);\nTypography::lead(\"Lead paragraph\").show(ui);\nTypography::muted(\"Muted text\").show(ui);\n\n// Custom variant\nTypography::new(\"Plain paragraph with Large variant.\")\n    .variant(TypographyVariant::Large)\n    .show(ui);",
        );
    }

    pub(crate) fn demo_label(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Labels pair with inputs in forms and settings.").show(ui);
        ui.add_space(12.0);
        _ = Flex::column().gap(8.0).show(ui, |f| {
            _ = f.ui(|ui51| {
                _ = Label::new("Your email address").show(ui51);
            });
            _ = f.add(
                functora_egui::Input::new(&mut self.flex_email).placeholder("you@example.com"),
            );
            _ = f.ui(|ui52| {
                _ = Label::new("Sizes").show(ui52);
            });
            _ = f.ui(|ui53| {
                _ = Label::new("Small label")
                    .size(functora_egui::ComponentSize::Sm)
                    .show(ui53);
            });
            _ = f.ui(|ui54| {
                _ = Label::new("Muted label").muted().show(ui54);
            });
        });

        snippet(
            ui,
            "// Label: text labels for forms\nuse functora_egui::{Label, Input, ComponentSize, Flex};\n\nLabel::new(\"Your email address\").show(ui);\n\nInput::new(&mut email).placeholder(\"you@example.com\").show(ui);\n\n// Sizes\nLabel::new(\"Small label\").size(ComponentSize::Sm).show(ui);\nLabel::new(\"Muted label\").muted().show(ui);",
        );
    }

    pub(crate) fn demo_kbd(ui: &mut egui::Ui) {
        _ = Typography::muted("Keyboard hint chips for shortcuts.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Ctrl"));
            _ = f.ui(|ui55| {
                _ = ui55.label("+");
            });
            _ = f.add(Kbd::new("K"));
            _ = f.ui(|ui56| {
                _ = ui56.label("opens the command palette");
            });
        });
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Shift"));
            _ = f.ui(|ui57| {
                _ = ui57.label("+");
            });
            _ = f.add(Kbd::new("Tab"));
            _ = f.ui(|ui58| {
                _ = ui58.label("cycles focus");
            });
        });

        snippet(
            ui,
            "// Kbd: keyboard hint chips\nuse functora_egui::{Kbd, Flex};\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Ctrl\"));\n    f.label(\"+\");\n    f.add(Kbd::new(\"K\"));\n    f.label(\"opens command palette\");\n});\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Shift\"));\n    f.label(\"+\");\n    f.add(Kbd::new(\"Tab\"));\n    f.label(\"cycles focus\");\n});",
        );
    }

    pub(crate) fn demo_item(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable rows for lists and menus.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Default variant").show(ui);
        ui.add_space(4.0);
        for (title, desc) in [
            ("Notifications", "Check your activity and updates"),
            ("Appearance", "Choose a theme for the app"),
            ("Storage", "Manage files and downloads"),
        ] {
            if Item::new()
                .show(ui, |ui17| {
                    _ = ui17.vertical(|ui18| {
                        _ = Label::new(title).show(ui18);
                        _ = ui18.label(desc);
                    });
                })
                .clicked()
            {
                self.toast.add(
                    format!("Item: {title}"),
                    functora_egui::ToastVariant::Default,
                    ui.ctx().input(|i| i.time),
                );
            }
        }
        ui.add_space(12.0);
        _ = Typography::small("Outline variant with icons").show(ui);
        ui.add_space(4.0);
        if Item::new()
            .variant(functora_egui::ItemVariant::Outline)
            .show(ui, |ui19| {
                _ = ui19.horizontal(|ui20| {
                    _ = Button::icon_only(LucideIcon::Settings)
                        .variant(ButtonVariant::Ghost)
                        .size(functora_egui::ComponentSize::Sm)
                        .show(ui20);
                    _ = ui20.label("Open settings");
                });
            })
            .clicked()
        {
            self.toast.add(
                "Settings",
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// Item: clickable rows for lists/menus\nuse functora_egui::{Item, Label, Button, LucideIcon, ButtonVariant, ComponentSize};\n\n// Default variant\nItem::new().show(ui, |item| {\n    item.vertical(|v| {\n        v.add(Label::new(\"Notifications\"));\n        v.label(\"Check your activity and updates\");\n    });\n});\n\n// Outline variant with icons\nItem::new().variant(ItemVariant::Outline).show(ui, |item| {\n    item.horizontal(|h| {\n        h.add(Button::icon_only(LucideIcon::Settings).variant(ButtonVariant::Ghost).size(ComponentSize::Sm));\n        h.label(\"Open settings\");\n    });\n});",
        );
    }

    pub(crate) fn demo_icons(&mut self, ui: &mut egui::Ui) {
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
            "// Icons: 1600+ Lucide icons from built-in SVG paths\nuse functora_egui::{LucideIcon, Button, ButtonVariant, ComponentSize};\n\n// Search and display icons\nlet search = \"settings\";\nlet icons: Vec<LucideIcon> = LucideIcon::ALL\n    .iter()\n    .copied()\n    .filter(|icon| icon.name().to_lowercase().contains(search))\n    .collect();\n\nfor icon in icons {\n    Button::icon_only(icon)\n        .variant(ButtonVariant::Ghost)\n        .size(ComponentSize::Sm)\n        .on_hover_text(icon.name())\n        .show(ui);\n}\n\n// Icons render from built-in SVG; no external font needed",
        );
    }
}
