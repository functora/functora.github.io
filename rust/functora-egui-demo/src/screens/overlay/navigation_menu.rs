use crate::catalog::NavSection;
use functora_egui::NavigationMenuValue;
use functora_egui::snippet;

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_navigation_menu(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Top-level navigation with active item tracking.");
        let entries = [
            (NavSection::Overview, "Overview".to_owned()),
            (NavSection::Integrations, "Integrations".to_owned()),
            (NavSection::Settings, "Settings".to_owned()),
        ];
        let clicked = NavigationMenuValue::new(&entries).show(ui, &mut self.nav_section);
        if let Some(section) = clicked {
            self.toast.add(
                format!("Navigation: {section:?}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        ui.add_space(12.0);
        crate::screens::section(ui, "String-list navigation");
        let clicked_index = functora_egui::NavigationMenu::new(vec![
            "Home".to_owned(),
            "Docs".to_owned(),
            "Blog".to_owned(),
        ])
        .show(ui, &mut self.nav_menu_index);
        if let Some(index) = clicked_index {
            self.toast.add(
                format!("Navigation index: {index}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = functora_egui::Typography::small(format!("Active index: {}", self.nav_menu_index))
            .show(ui);

        snippet(
            ui,
            "// NavigationMenuValue: top-level navigation bound to an enum\nuse functora_egui::NavigationMenuValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum NavSection { Overview, Integrations, Settings }\n\nlet entries = [(NavSection::Overview, \"Overview\".to_owned()), (NavSection::Integrations, \"Integrations\".to_owned()), (NavSection::Settings, \"Settings\".to_owned())];\nlet mut active = NavSection::Overview;\n\nif let Some(section) = NavigationMenuValue::new(&entries).show(ui, &mut active) {\n    eprintln!(\"Navigated to: {section:?}\");\n}\n\n// NavigationMenu: the string-list variant tracks the active index\nuse functora_egui::NavigationMenu;\n\nlet mut active = 0usize;\nif let Some(index) = NavigationMenu::new(vec![\"Home\".to_owned(), \"Docs\".to_owned()])\n    .show(ui, &mut active)\n{\n    eprintln!(\"Navigated to index {index}\");\n}",
        );
    }
}
