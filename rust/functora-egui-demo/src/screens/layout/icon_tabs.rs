use crate::catalog::ProfileTab;
use functora_egui::snippet;
use functora_egui::{IconTabs, IconTabsValue, LucideIcon, TabEntry};

impl crate::state::ShowcaseApp {
    pub fn demo_icon_tabs(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Icon-based tabs with tooltips.");
        let entries = [
            (
                ProfileTab::Home,
                TabEntry::Icon {
                    icon: LucideIcon::House,
                    tooltip: "Home".to_owned(),
                },
            ),
            (
                ProfileTab::Settings,
                TabEntry::Icon {
                    icon: LucideIcon::Settings,
                    tooltip: "Settings".to_owned(),
                },
            ),
            (
                ProfileTab::Profile,
                TabEntry::Icon {
                    icon: LucideIcon::CircleUser,
                    tooltip: "Profile".to_owned(),
                },
            ),
            (
                ProfileTab::Notifications,
                TabEntry::Icon {
                    icon: LucideIcon::Bell,
                    tooltip: "Notifications".to_owned(),
                },
            ),
        ];
        _ = IconTabsValue::new(&entries).show(ui, &mut self.profile_tab, |ui77, tab| match tab {
            ProfileTab::Home => {
                _ = ui77.label("Home content");
            }
            ProfileTab::Settings => {
                _ = ui77.label("Settings content");
            }
            ProfileTab::Profile => {
                _ = ui77.label("Profile content");
            }
            ProfileTab::Notifications => {
                _ = ui77.label("Notifications content");
            }
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Index-based icon tabs");
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
                icon: LucideIcon::Bell,
                tooltip: "Notifications".to_owned(),
            },
        ])
        .show(ui, &mut self.profile_tab_index, |tab_ui, index| {
            _ = tab_ui.label(match index {
                0 => "Home content",
                1 => "Settings content",
                _ => "Notifications content",
            });
        });

        snippet(
            ui,
            "// IconTabsValue: icon-only tabs bound to an enum\nuse functora_egui::{IconTabsValue, TabEntry, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum ProfileTab { Home, Settings, Profile, Notifications }\n\nlet entries = [\n    (ProfileTab::Home, TabEntry::Icon { icon: LucideIcon::House, tooltip: \"Home\".to_owned() }),\n    (ProfileTab::Settings, TabEntry::Icon { icon: LucideIcon::Settings, tooltip: \"Settings\".to_owned() }),\n    (ProfileTab::Profile, TabEntry::Icon { icon: LucideIcon::CircleUser, tooltip: \"Profile\".to_owned() }),\n    (ProfileTab::Notifications, TabEntry::Icon { icon: LucideIcon::Bell, tooltip: \"Notifications\".to_owned() }),\n];\nlet mut active = ProfileTab::Home;\nIconTabsValue::new(&entries).show(ui, &mut active, |content, tab| {\n    match tab {\n        ProfileTab::Home => content.label(\"Home content\"),\n        ProfileTab::Settings => content.label(\"Settings content\"),\n        ProfileTab::Profile => content.label(\"Profile content\"),\n        ProfileTab::Notifications => content.label(\"Notifications content\"),\n    }\n});\n\n// IconTabs: the index-based variant\nuse functora_egui::IconTabs;\n\nlet mut active = 0usize;\nIconTabs::new(vec![\n    TabEntry::Icon { icon: LucideIcon::House, tooltip: \"Home\".to_owned() },\n    TabEntry::Icon { icon: LucideIcon::Settings, tooltip: \"Settings\".to_owned() },\n])\n.show(ui, &mut active, |content, index| {\n    content.label(match index {\n        0 => \"Home content\",\n        _ => \"Settings content\",\n    });\n});",
        );
    }
}
