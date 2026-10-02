use crate::catalog::ProfileTab;
use functora_egui::snippet;
use functora_egui::{IconTabsValue, LucideIcon, TabEntry, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_icon_tabs(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Icon-based tabs with tooltips.").show(ui);
        ui.add_space(12.0);
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

        snippet(
            ui,
            "// IconTabsValue: icon-only tabs bound to an enum\nuse functora_egui::{IconTabsValue, TabEntry, LucideIcon};\n\n#[derive(Clone, Copy, PartialEq)]\nenum ProfileTab { Home, Settings, Profile, Notifications }\n\nlet entries = [\n    (ProfileTab::Home, TabEntry::Icon { icon: LucideIcon::House, tooltip: \"Home\".to_owned() }),\n    (ProfileTab::Settings, TabEntry::Icon { icon: LucideIcon::Settings, tooltip: \"Settings\".to_owned() }),\n    (ProfileTab::Profile, TabEntry::Icon { icon: LucideIcon::CircleUser, tooltip: \"Profile\".to_owned() }),\n    (ProfileTab::Notifications, TabEntry::Icon { icon: LucideIcon::Bell, tooltip: \"Notifications\".to_owned() }),\n];\nlet mut active = ProfileTab::Home;\nIconTabsValue::new(&entries).show(ui, &mut active, |content, tab| {\n    match tab {\n        ProfileTab::Home => content.label(\"Home content\"),\n        ProfileTab::Settings => content.label(\"Settings content\"),\n        ProfileTab::Profile => content.label(\"Profile content\"),\n        ProfileTab::Notifications => content.label(\"Notifications content\"),\n    }\n});",
        );
    }
}
