use crate::catalog::SettingsTab;
use functora_egui::Tabs;
use functora_egui::TabsValue;
use functora_egui::snippet;

impl crate::state::ShowcaseApp {
    pub fn demo_tabs(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Tabbed content panels.");
        let entries = [
            (SettingsTab::Account, "Account".to_owned()),
            (SettingsTab::Password, "Password".to_owned()),
            (SettingsTab::Settings, "Settings".to_owned()),
        ];
        _ = TabsValue::new(&entries).show(ui, &mut self.settings_tab, |ui76, tab| match tab {
            SettingsTab::Account => {
                _ = ui76.label("Manage your account settings and preferences.");
            }
            SettingsTab::Password => {
                _ = ui76.label("Change your password and security settings.");
            }
            SettingsTab::Settings => {
                _ = ui76.label("Configure application settings.");
            }
        });
        _ = TabsValue::new(&entries)
            .fill_width()
            .show(ui, &mut self.settings_tab, |ui77, tab| match tab {
                SettingsTab::Account => {
                    _ = ui77.label("Equal-width account tab.");
                }
                SettingsTab::Password => {
                    _ = ui77.label("Equal-width password tab.");
                }
                SettingsTab::Settings => {
                    _ = ui77.label("Equal-width settings tab.");
                }
            });

        ui.add_space(12.0);
        crate::screens::section(ui, "Index-based tabs");
        _ = Tabs::new(vec![
            "Overview".to_owned(),
            "Analytics".to_owned(),
            "Reports".to_owned(),
        ])
        .show(ui, &mut self.settings_tab_index, |tab_ui, index| {
            _ = tab_ui.label(match index {
                0 => "Overview panel",
                1 => "Analytics panel",
                _ => "Reports panel",
            });
        });

        snippet(
            ui,
            "// TabsValue: tabbed content panels bound to an enum\nuse functora_egui::TabsValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum SettingsTab { Account, Password, Settings }\n\nlet entries = [(SettingsTab::Account, \"Account\".to_owned()), (SettingsTab::Password, \"Password\".to_owned()), (SettingsTab::Settings, \"Settings\".to_owned())];\nlet mut active = SettingsTab::Account;\n\nTabsValue::new(&entries).show(ui, &mut active, |content, tab| {\n    match tab {\n        SettingsTab::Account => content.label(\"Manage your account settings and preferences.\"),\n        SettingsTab::Password => content.label(\"Change your password and security settings.\"),\n        SettingsTab::Settings => content.label(\"Configure application settings.\"),\n    }\n});\n\n// Stretched tab bar with equal-width tabs\nTabsValue::new(&entries).fill_width().show(ui, &mut active, |content, tab| {\n    match tab {\n        SettingsTab::Account => content.label(\"Equal-width account tab.\"),\n        SettingsTab::Password => content.label(\"Equal-width password tab.\"),\n        SettingsTab::Settings => content.label(\"Equal-width settings tab.\"),\n    }\n});\n\n// Tabs: the string-list variant selects by index\nuse functora_egui::Tabs;\n\nlet mut active = 0usize;\nTabs::new(vec![\"Overview\".to_owned(), \"Analytics\".to_owned()])\n    .show(ui, &mut active, |content, index| {\n        content.label(match index {\n            0 => \"Overview panel\",\n            _ => \"Analytics panel\",\n        });\n    });",
        );
    }
}
