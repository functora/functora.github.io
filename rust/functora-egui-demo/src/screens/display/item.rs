use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Item, Label, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_item(&mut self, ui: &mut egui::Ui) {
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
            "// Item: clickable rows for lists/menus\nuse functora_egui::{Item, Label, Button, LucideIcon, ButtonVariant, ComponentSize, ItemVariant};\n\n// Default variant\nfor (title, desc) in [\n    (\"Notifications\", \"Check your activity and updates\"),\n    (\"Appearance\", \"Choose a theme for the app\"),\n    (\"Storage\", \"Manage files and downloads\"),\n] {\n    Item::new().show(ui, |item| {\n        item.vertical(|v| {\n            v.add(Label::new(title));\n            v.label(desc);\n        });\n    });\n}\n\n// Outline variant with icons\nItem::new().variant(ItemVariant::Outline).show(ui, |item| {\n    item.horizontal(|h| {\n        h.add(Button::icon_only(LucideIcon::Settings).variant(ButtonVariant::Ghost).size(ComponentSize::Sm));\n        h.label(\"Open settings\");\n    });\n});",
        );
    }
}
