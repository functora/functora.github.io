use functora_egui::snippet;
use functora_egui::{
    Badge, BadgeVariant, Button, ButtonGroup, ButtonVariant, ComponentSize, LucideIcon, Toolbar,
    Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_toolbar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Compact command container for editor and app controls.").show(ui);
        ui.add_space(12.0);

        _ = Toolbar::new().show(ui, |ui64| {
            _ = ButtonGroup::show(ui64, |ui65| {
                for (tool, icon) in crate::catalog::Tool::ALL {
                    let response = Button::icon_only(icon)
                        .variant(ButtonVariant::Ghost)
                        .selected(self.toolbar.toolbar_tool == tool)
                        .show(ui65);
                    if response.clicked() {
                        self.toolbar.toolbar_tool = tool;
                    }
                }
            });
            _ = ButtonGroup::show(ui64, |ui66| {
                _ = Button::icon_only(LucideIcon::Undo2)
                    .variant(ButtonVariant::Ghost)
                    .show(ui66);
                _ = Button::icon_only(LucideIcon::Redo2)
                    .variant(ButtonVariant::Ghost)
                    .show(ui66);
            });
            if Button::new("Snap")
                .variant(ButtonVariant::Outline)
                .selected(self.toolbar.toolbar_snap)
                .shortcut_text("S")
                .show(ui64)
                .clicked()
            {
                self.toolbar.toolbar_snap = !self.toolbar.toolbar_snap;
            }
            _ = Button::new("Preview").icon(LucideIcon::Play).show(ui64);
        });

        ui.add_space(12.0);
        _ = Typography::small("Dense toolbar").show(ui);
        ui.add_space(4.0);
        _ = Toolbar::new().dense().wrap(false).show(ui, |ui67| {
            _ = Button::icon_only(LucideIcon::ZoomOut)
                .variant(ButtonVariant::Ghost)
                .size(ComponentSize::Sm)
                .show(ui67);
            _ = Badge::new("100%")
                .variant(BadgeVariant::Secondary)
                .show(ui67);
            _ = Button::icon_only(LucideIcon::ZoomIn)
                .variant(ButtonVariant::Ghost)
                .size(ComponentSize::Sm)
                .show(ui67);
        });

        snippet(
            ui,
            "// Toolbar: tool selection bound to an enum\nuse functora_egui::{Toolbar, ButtonGroup, Button, ButtonVariant, LucideIcon, Badge, BadgeVariant, ComponentSize};\n\n#[derive(Clone, Copy, PartialEq)]\nenum Tool { Select, Pen, Spline, Frame, Text }\n\nlet tools = [(Tool::Select, LucideIcon::MousePointer2), (Tool::Pen, LucideIcon::PenTool), (Tool::Spline, LucideIcon::Spline), (Tool::Frame, LucideIcon::Frame), (Tool::Text, LucideIcon::Type)];\nlet mut tool = Tool::Select;\n\nToolbar::new().show(ui, |bar| {\n    ButtonGroup::show(bar, |bg| {\n        for (value, icon) in tools {\n            if Button::icon_only(icon)\n                .variant(ButtonVariant::Ghost)\n                .selected(tool == value)\n                .show(bg)\n                .clicked()\n            {\n                tool = value;\n            }\n        }\n    });\n    ButtonGroup::show(bar, |bg| {\n        Button::icon_only(LucideIcon::Undo2).variant(ButtonVariant::Ghost).show(bg);\n        Button::icon_only(LucideIcon::Redo2).variant(ButtonVariant::Ghost).show(bg);\n    });\n    Button::new(\"Snap\").variant(ButtonVariant::Outline).selected(snap).shortcut_text(\"S\").show(bar);\n    Button::new(\"Preview\").icon(LucideIcon::Play).show(bar);\n});\n\n// Dense toolbar\nToolbar::new().dense().wrap(false).show(ui, |bar| {\n    Button::icon_only(LucideIcon::ZoomOut).variant(ButtonVariant::Ghost).size(ComponentSize::Sm).show(bar);\n    Badge::new(\"100%\").variant(BadgeVariant::Secondary).show(bar);\n    Button::icon_only(LucideIcon::ZoomIn).variant(ButtonVariant::Ghost).size(ComponentSize::Sm).show(bar);\n});",
        );
    }
}
