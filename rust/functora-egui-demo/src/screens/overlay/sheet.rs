use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, LucideIcon, SheetSide, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_sheet(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A side panel that slides in from the edge.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("On mobile the sheet opens from the bottom.").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(
                Button::new("Right")
                    .icon(LucideIcon::PanelRight)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Right;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Left")
                    .icon(LucideIcon::PanelLeft)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Left;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Top")
                    .icon(LucideIcon::PanelTop)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Top;
                self.sheet_state.sheet_open = true;
            }
            if f.add(
                Button::new("Bottom")
                    .icon(LucideIcon::PanelBottom)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.sheet_state.sheet_side = SheetSide::Bottom;
                self.sheet_state.sheet_open = true;
            }
        });

        snippet(
            ui,
            "// Sheet: side panel from edge (right/left/top/bottom)\nuse functora_egui::{Sheet, SheetSide, Button, ButtonVariant, LucideIcon, Label, Item, FieldDescription, Flex};\n\nlet mut open = false;\nlet mut side = SheetSide::Right;\n\nFlex::row().gap(8.0).show(ui, |f| {\n    if f.add(Button::new(\"Right\").icon(LucideIcon::PanelRight).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Right;\n        open = true;\n    }\n    if f.add(Button::new(\"Left\").icon(LucideIcon::PanelLeft).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Left;\n        open = true;\n    }\n    if f.add(Button::new(\"Top\").icon(LucideIcon::PanelTop).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Top;\n        open = true;\n    }\n    if f.add(Button::new(\"Bottom\").icon(LucideIcon::PanelBottom).variant(ButtonVariant::Outline)).inner.clicked() {\n        side = SheetSide::Bottom;\n        open = true;\n    }\n});\n\nSheet::new()\n    .title(\"Sheet Panel\")\n    .description(\"A side sheet that slides in from the edge.\")\n    .side(side)\n    .show(ctx, &mut open, |ui| {\n        Label::new(\"Notifications\").show(ui);\n        ui.add_space(4.0);\n        for (label, desc) in [\n            (\"New comment\", \"Alice commented on your post.\"),\n            (\"Build passed\", \"The release pipeline finished.\"),\n            (\"Update ready\", \"functora-egui 0.2 is available.\"),\n        ] {\n            Item::new().show(ui, |item| {\n                item.vertical(|v| {\n                    Label::new(label).show(v);\n                    FieldDescription::show(v, desc);\n                });\n            });\n        }\n    });\n\n// On mobile, opens from bottom regardless of side",
        );
    }
}
