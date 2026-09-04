//! Layout: flex, aspect ratio, cards, collapsible, resizable, scroll areas,
//! separators, status bars, tabs, toolbars, accordions.

use functora_egui::{
    Accordion, AspectRatio, Badge, BadgeVariant, Button, ButtonGroup, ButtonVariant, Card,
    Collapsible, ComponentSize, Flex, IconTabs, Input, Kbd, Label, LucideIcon, Resizable,
    ScrollArea, Separator, StatusBar, TabEntry, Tabs, Toolbar, Typography,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_flex(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Flexbox-like layout with gap, grow, justify, align, wrap, and spacer.",
        )
        .show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Row with gap").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Cancel").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Save"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Column with gap").show(ui);
        ui.add_space(4.0);
        _ = Flex::column().gap(8.0).show(ui, |f| {
            _ = f.add(Badge::new("First"));
            _ = f.add(Badge::new("Second"));
            _ = f.add(Badge::new("Third"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Grow: input fills, button stays natural").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).w_full().show(ui, |f| {
            _ = f.grow(
                1.0,
                Input::new(&mut self.flex_input).placeholder("Type a message..."),
            );
            _ = f.add(Button::new("Send"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Justify end").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().justify_end().gap(8.0).w_full().show(ui, |f| {
            _ = f.add(Button::new("Cancel").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Confirm"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Justify between").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().justify_between().w_full().show(ui, |f| {
            _ = f.add(Button::new("Previous").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Next"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Justify center").show(ui);
        ui.add_space(4.0);
        _ = Flex::row()
            .justify_center()
            .gap(8.0)
            .w_full()
            .show(ui, |f| {
                _ = f.add(functora_egui::Spinner::new().size(20.0));
                _ = f.ui(|ui69| {
                    _ = ui69.label("Loading...");
                });
            });

        ui.add_space(16.0);
        _ = Typography::small("Wrap: overflowing items wrap to the next line").show(ui);
        ui.add_space(4.0);
        let tags = [
            "Rust",
            "egui",
            "shadcn",
            "flexbox",
            "layout",
            "widgets",
            "responsive",
            "wrap",
            "gap",
            "grow",
            "theming",
            "buttons",
            "inputs",
            "cards",
            "dialogs",
            "toasts",
            "badges",
        ];
        _ = Flex::row().gap(4.0).wrap().w_full().show(ui, |f| {
            for tag in tags {
                _ = f.add(Badge::new(tag));
            }
        });

        ui.add_space(16.0);
        _ = Typography::small("Spacer: pushes items apart").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).w_full().show(ui, |f| {
            _ = f.add(Badge::new("Left"));
            _ = f.spacer();
            _ = f.add(Badge::new("Right"));
        });

        ui.add_space(16.0);
        _ = Typography::small("Nested flex: two-column form").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(16.0).w_full().show(ui, |f| {
            _ = f.grow_nested(1.0, Flex::column().gap(8.0), |f4| {
                _ = f4.ui(|ui70| {
                    _ = Label::new("First Name").show(ui70);
                });
                _ = f4.add(Input::new(&mut self.flex_first).placeholder("John"));
                _ = f4.ui(|ui71| {
                    _ = Label::new("Last Name").show(ui71);
                });
                _ = f4.add(Input::new(&mut self.flex_last).placeholder("Doe"));
            });
            _ = f.grow_nested(1.0, Flex::column().gap(8.0), |f5| {
                _ = f5.ui(|ui72| {
                    _ = Label::new("Email").show(ui72);
                });
                _ = f5.add(Input::new(&mut self.flex_email).placeholder("john@example.com"));
                _ = f5.ui(|ui73| {
                    _ = Label::new("Phone").show(ui73);
                });
                _ = f5.add(Input::new(&mut self.flex_phone).placeholder("+1 555-1234"));
            });
        });

        ui.add_space(16.0);
        _ = Typography::small("Center utility").show(ui);
        ui.add_space(4.0);
        _ = egui::Frame::NONE
            .stroke(egui::Stroke::new(1.0, egui::Color32::from_gray(80)))
            .show(ui, |ui47| {
                ui47.set_min_height(80.0);
                _ = functora_egui::center(ui47, |ui48| {
                    _ = ui48.horizontal(|ui49| {
                        _ = functora_egui::Spinner::new().size(16.0).show(ui49);
                        ui49.add_space(8.0);
                        _ = ui49.label("Centered content");
                    });
                });
            });

        snippet(
            ui,
            "// Flex: flexbox-like layout with gap, grow, justify, align, wrap\nuse functora_egui::Flex;\n\n// Row with gap\nFlex::row().gap(8.0).show(ui, |f| {\n    f.add(Button::new(\"Cancel\").variant(ButtonVariant::Outline));\n    f.add(Button::new(\"Save\"));\n});\n\n// Column with gap\nFlex::column().gap(8.0).show(ui, |f| {\n    f.add(Badge::new(\"First\"));\n    f.add(Badge::new(\"Second\"));\n});\n\n// Grow: input fills, button stays natural\nFlex::row().gap(8.0).w_full().show(ui, |f| {\n    f.grow(1.0, Input::new(&mut text).placeholder(\"Type...\"));\n    f.add(Button::new(\"Send\"));\n});\n\n// Justify between\nFlex::row().justify_between().w_full().show(ui, |f| {\n    f.add(Button::new(\"Previous\"));\n    f.add(Button::new(\"Next\"));\n});\n\n// Spacer pushes items apart\nFlex::row().gap(8.0).w_full().show(ui, |f| {\n    f.add(Badge::new(\"Left\"));\n    f.spacer();\n    f.add(Badge::new(\"Right\"));\n});\n\n// Wrap\nFlex::row().gap(4.0).wrap().w_full().show(ui, |f| {\n    for tag in [\"Rust\", \"egui\", \"flex\"] {\n        f.add(Badge::new(tag));\n    }\n});",
        );
    }

    pub(crate) fn demo_aspect_ratio(ui: &mut egui::Ui) {
        _ = Typography::muted("Maintains a fixed width-to-height ratio.").show(ui);
        ui.add_space(12.0);
        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
        ui.set_max_width(ui.available_width());
        _ = AspectRatio::new(16.0 / 9.0).show(ui, |ui50| {
            let rect = ui50.available_rect_before_wrap();
            _ = ui50.painter().rect_filled(
                rect,
                egui::CornerRadius::from(theme.radius),
                theme.muted,
            );
            let galley = ui50.painter().layout_no_wrap(
                "16:9".to_owned(),
                egui::FontId::proportional(20.0),
                theme.muted_foreground,
            );
            ui50.painter().galley(
                egui::pos2(
                    rect.center().x - galley.size().x / 2.0,
                    rect.center().y - galley.size().y / 2.0,
                ),
                galley,
                theme.muted_foreground,
            );
        });
        ui.add_space(8.0);
        _ = AspectRatio::new(1.0).show(ui, |ui51| {
            let rect = ui51.available_rect_before_wrap();
            _ = ui51.painter().rect_filled(
                rect,
                egui::CornerRadius::from(theme.radius),
                theme.muted,
            );
            let galley = ui51.painter().layout_no_wrap(
                "1:1".to_owned(),
                egui::FontId::proportional(20.0),
                theme.muted_foreground,
            );
            ui51.painter().galley(
                egui::pos2(
                    rect.center().x - galley.size().x / 2.0,
                    rect.center().y - galley.size().y / 2.0,
                ),
                galley,
                theme.muted_foreground,
            );
        });

        snippet(
            ui,
            "// AspectRatio: maintains fixed width/height ratio\nuse functora_egui::AspectRatio;\n\n// 16:9 video player\nAspectRatio::new(16.0 / 9.0).show(ui, |ui| {\n    // ui.available_rect_before_wrap() is 16:9\n    ui.label(\"Video player area\");\n});\n\n// 1:1 square\nAspectRatio::new(1.0).show(ui, |ui| {\n    ui.label(\"Square thumbnail\");\n});",
        );
    }

    pub(crate) fn demo_card(ui: &mut egui::Ui) {
        _ = Typography::muted("Bordered container for grouping content.").show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui52| {
            _ = Typography::h4("Card Title").show(ui52);
            ui52.add_space(4.0);
            _ = ui52.label("This is a card with some descriptive content inside.");
            ui52.add_space(8.0);
            _ = Button::new("Action")
                .variant(ButtonVariant::Outline)
                .size(ComponentSize::Sm)
                .show(ui52);
        });
        ui.add_space(8.0);

        snippet(
            ui,
            "// Card: bordered container for grouping content\nuse functora_egui::{Card, Button, ButtonVariant, ComponentSize};\n\nCard::new().show(ui, |card| {\n    card.add(Typography::h4(\"Card Title\"));\n    card.add_space(4.0);\n    card.label(\"Content inside the card...\");\n    card.add_space(8.0);\n    Button::new(\"Action\")\n        .variant(ButtonVariant::Outline)\n        .size(ComponentSize::Sm)\n        .show(card);\n});",
        );
    }

    pub(crate) fn demo_collapsible(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A section that can be toggled open or closed.").show(ui);
        ui.add_space(12.0);
        _ = Collapsible::new("Click to toggle").show(
            ui,
            &mut self.checks.collapsible_open,
            |ui55| {
                _ = ui55.label("This content is hidden when the collapsible is closed.");
                _ = ui55.label("You can put any widgets inside here.");
                ui55.add_space(4.0);
                _ = Button::new("Nested Action")
                    .variant(ButtonVariant::Outline)
                    .size(ComponentSize::Sm)
                    .show(ui55);
            },
        );

        snippet(
            ui,
            "// Collapsible: toggleable content section\nuse functora_egui::Collapsible;\n\nlet mut open = true;\nCollapsible::new(\"Click to toggle\").show(ui, &mut open, |body| {\n    body.label(\"This content is hidden when closed.\");\n    body.label(\"You can put any widgets inside here.\");\n    Button::new(\"Nested Action\")\n        .variant(ButtonVariant::Outline)\n        .size(ComponentSize::Sm)\n        .show(body);\n});",
        );
    }

    pub(crate) fn demo_resizable(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Draggable split pane with an adjustable divider.").show(ui);
        ui.add_space(12.0);
        _ = Resizable::new().height(160.0).show(
            ui,
            &mut self.resizable_fraction,
            |ui56| {
                _ = Card::new().show(ui56, |ui57| {
                    _ = ui57.label("Left Panel");
                });
            },
            |ui58| {
                _ = Card::new().show(ui58, |ui59| {
                    _ = ui59.label("Right Panel");
                });
            },
        );
        ui.add_space(4.0);
        _ = Typography::small(format!("Fraction: {:.2}", self.resizable_fraction)).show(ui);

        snippet(
            ui,
            "// Resizable: draggable split pane\nuse functora_egui::Resizable;\n\nlet mut fraction = 0.5;\nResizable::new()\n    .height(160.0)\n    .show(ui, &mut fraction, |left| {\n        Card::new().show(left, |l| l.label(\"Left Panel\"));\n    }, |right| {\n        Card::new().show(right, |r| r.label(\"Right Panel\"));\n    });\n\n// fraction is now the split ratio (0.0 to 1.0)",
        );
    }

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

    pub(crate) fn demo_separator(ui: &mut egui::Ui) {
        _ = Typography::muted("Visual divider between content sections.").show(ui);
        ui.add_space(12.0);
        _ = ui.label("Content above");
        _ = Separator::horizontal().show(ui);
        _ = ui.label("Content below");
        ui.add_space(12.0);
        _ = Separator::horizontal().text("With Label").show(ui);
        _ = ui.label("Content after labeled separator");
        ui.add_space(12.0);
        _ = ui.horizontal(|ui61| {
            _ = ui61.label("Left");
            _ = Separator::vertical().show(ui61);
            _ = ui61.label("Right");
        });

        snippet(
            ui,
            "// Separator: visual dividers\nuse functora_egui::Separator;\n\nSeparator::horizontal().show(ui);\nSeparator::horizontal().text(\"With Label\").show(ui);\nui.horizontal(|ui| {\n    ui.label(\"Left\");\n    Separator::vertical().show(ui);\n    ui.label(\"Right\");\n});",
        );
    }

    pub(crate) fn demo_status_bar(ui: &mut egui::Ui) {
        _ = Typography::muted("Compact container for workspace state and metadata.").show(ui);
        ui.add_space(12.0);
        _ = StatusBar::new().show(ui, |ui62| {
            _ = Badge::new("Saved")
                .variant(BadgeVariant::Secondary)
                .show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Typography::small("Canvas 1920 x 1080").show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Typography::small("2 objects selected").show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Kbd::new("Cmd").show(ui62);
            _ = ui62.label("+");
            _ = Kbd::new("S").show(ui62);
        });
        ui.add_space(12.0);
        _ = StatusBar::new().dense().show(ui, |ui63| {
            _ = Typography::small("x: 124").show(ui63);
            _ = Typography::small("y: 88").show(ui63);
            _ = Typography::small("rotation: -8deg").show(ui63);
            _ = Badge::new("Snapping")
                .variant(BadgeVariant::Outline)
                .show(ui63);
        });

        snippet(
            ui,
            "// StatusBar: compact container for workspace state\nuse functora_egui::{StatusBar, Badge, BadgeVariant, Separator, Kbd, Typography};\n\nStatusBar::new().show(ui, |bar| {\n    Badge::new(\"Saved\").variant(BadgeVariant::Secondary).show(bar);\n    Separator::vertical().show(bar);\n    Typography::small(\"Canvas 1920 x 1080\").show(bar);\n    Separator::vertical().show(bar);\n    Typography::small(\"2 objects selected\").show(bar);\n    Separator::vertical().show(bar);\n    Kbd::new(\"Cmd\").show(bar);\n    bar.label(\"+\");\n    Kbd::new(\"S\").show(bar);\n});\n\n// Dense variant\nStatusBar::new().dense().show(ui, |bar| {\n    Typography::small(\"x: 124\").show(bar);\n    Typography::small(\"y: 88\").show(bar);\n    Typography::small(\"rotation: -8deg\").show(bar);\n    Badge::new(\"Snapping\").variant(BadgeVariant::Outline).show(bar);\n});",
        );
    }

    pub(crate) fn demo_tabs(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Tabbed content panels.").show(ui);
        ui.add_space(12.0);
        _ = Tabs::new(vec![
            "Account".to_owned(),
            "Password".to_owned(),
            "Settings".to_owned(),
        ])
        .show(ui, &mut self.tabs_idx, |ui76, idx| match idx {
            0 => {
                _ = ui76.label("Manage your account settings and preferences.");
            }
            1 => {
                _ = ui76.label("Change your password and security settings.");
            }
            _ => {
                _ = ui76.label("Configure application settings.");
            }
        });

        snippet(
            ui,
            "// Tabs: tabbed content panels\nuse functora_egui::Tabs;\n\nlet titles = vec![\"Account\", \"Password\", \"Settings\"];\nlet mut active = 0;\n\nTabs::new(titles).show(ui, &mut active, |content, idx| {\n    match idx {\n        0 => content.label(\"Account settings...\"),\n        1 => content.label(\"Password settings...\"),\n        _ => content.label(\"App settings...\"),\n    }\n});",
        );
    }

    pub(crate) fn demo_icon_tabs(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Icon-based tabs with tooltips.").show(ui);
        ui.add_space(12.0);
        _ = IconTabs::new(vec![
            TabEntry::Icon {
                icon: LucideIcon::House,
                tooltip: "Home".to_owned(),
            },
            TabEntry::Icon {
                icon: LucideIcon::Settings,
                tooltip: "Settings".to_owned(),
            },
            TabEntry::Icon {
                icon: LucideIcon::CircleUser,
                tooltip: "Profile".to_owned(),
            },
            TabEntry::Icon {
                icon: LucideIcon::Bell,
                tooltip: "Notifications".to_owned(),
            },
        ])
        .show(ui, &mut self.icon_tabs_idx, |ui77, idx| match idx {
            0 => {
                _ = ui77.label("Home content");
            }
            1 => {
                _ = ui77.label("Settings content");
            }
            2 => {
                _ = ui77.label("Profile content");
            }
            _ => {
                _ = ui77.label("Notifications content");
            }
        });

        snippet(
            ui,
            "// IconTabs: icon-only tabs with tooltips\nuse functora_egui::{IconTabs, TabEntry, LucideIcon};\n\nlet entries = vec![\n    TabEntry::Icon { icon: LucideIcon::House, tooltip: \"Home\".to_owned() },\n    TabEntry::Icon { icon: LucideIcon::Settings, tooltip: \"Settings\".to_owned() },\n    TabEntry::Icon { icon: LucideIcon::CircleUser, tooltip: \"Profile\".to_owned() },\n    TabEntry::Icon { icon: LucideIcon::Bell, tooltip: \"Notifications\".to_owned() },\n];\nlet mut active = 0;\nIconTabs::new(entries).show(ui, &mut active, |content, idx| {\n    match idx {\n        0 => content.label(\"Home content\"),\n        1 => content.label(\"Settings content\"),\n        2 => content.label(\"Profile content\"),\n        _ => content.label(\"Notifications content\"),\n    }\n});",
        );
    }

    pub(crate) fn demo_toolbar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Compact command container for editor and app controls.").show(ui);
        ui.add_space(12.0);

        let tools = [
            LucideIcon::MousePointer2,
            LucideIcon::PenTool,
            LucideIcon::Spline,
            LucideIcon::Frame,
            LucideIcon::Type,
        ];
        _ = Toolbar::new().show(ui, |ui64| {
            _ = ButtonGroup::show(ui64, |ui65| {
                for (idx, icon) in tools.iter().enumerate() {
                    let response = Button::icon_only(*icon)
                        .variant(ButtonVariant::Ghost)
                        .selected(self.toolbar.toolbar_tool_idx == idx)
                        .show(ui65);
                    if response.clicked() {
                        self.toolbar.toolbar_tool_idx = idx;
                    }
                }
            });
            _ = Separator::vertical().show(ui64);
            _ = ButtonGroup::show(ui64, |ui66| {
                _ = Button::icon_only(LucideIcon::Undo2)
                    .variant(ButtonVariant::Ghost)
                    .show(ui66);
                _ = Button::icon_only(LucideIcon::Redo2)
                    .variant(ButtonVariant::Ghost)
                    .show(ui66);
            });
            _ = Separator::vertical().show(ui64);
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

        ui.add_space(14.0);
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
            "// Toolbar: compact command container\nuse functora_egui::{Toolbar, ButtonGroup, Button, ButtonVariant, LucideIcon, Separator, Badge, BadgeVariant, ComponentSize};\n\nlet tools = [LucideIcon::MousePointer2, LucideIcon::PenTool, LucideIcon::Spline];\n\nToolbar::new().show(ui, |bar| {\n    ButtonGroup::show(bar, |bg| {\n        for (idx, icon) in tools.iter().enumerate() {\n            Button::icon_only(*icon)\n                .variant(ButtonVariant::Ghost)\n                .selected(tool_idx == idx)\n                .show(bg);\n        }\n    });\n    Separator::vertical().show(bar);\n    ButtonGroup::show(bar, |bg| {\n        Button::icon_only(LucideIcon::Undo2).variant(ButtonVariant::Ghost).show(bg);\n        Button::icon_only(LucideIcon::Redo2).variant(ButtonVariant::Ghost).show(bg);\n    });\n    Separator::vertical().show(bar);\n    Button::new(\"Snap\").variant(ButtonVariant::Outline).selected(snap).show(bar);\n});\n\n// Dense toolbar\nToolbar::new().dense().wrap(false).show(ui, |bar| {\n    Button::icon_only(LucideIcon::ZoomOut).variant(ButtonVariant::Ghost).size(ComponentSize::Sm).show(bar);\n    Badge::new(\"100%\").variant(BadgeVariant::Secondary).show(bar);\n    Button::icon_only(LucideIcon::ZoomIn).variant(ButtonVariant::Ghost).size(ComponentSize::Sm).show(bar);\n});",
        );
    }

    pub(crate) fn demo_accordion(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Expandable sections: click to toggle.").show(ui);
        ui.add_space(12.0);
        _ = Accordion::new(vec![
            (
                "Is it accessible?".to_owned(),
                "Yes. It adheres to the WAI-ARIA design pattern.".to_owned(),
            ),
            (
                "Is it styled?".to_owned(),
                "Yes. It comes with default styles matching shadcn/ui.".to_owned(),
            ),
            (
                "Is it animated?".to_owned(),
                "Yes. It has smooth open/close transitions.".to_owned(),
            ),
        ])
        .multiple()
        .show(ui, &mut self.accordion_open);

        snippet(
            ui,
            "// Accordion: expandable sections\nuse functora_egui::Accordion;\n\nlet items = vec![\n    (\"Is it accessible?\", \"Yes. It adheres to WAI-ARIA.\"),\n    (\"Is it styled?\", \"Yes. Matches shadcn/ui.\"),\n    (\"Is it animated?\", \"Yes. Smooth open/close transitions.\"),\n];\nlet mut open_indices = vec![0];\n\nAccordion::new(items)\n    .multiple()\n    .show(ui, &mut open_indices);",
        );
    }
}
