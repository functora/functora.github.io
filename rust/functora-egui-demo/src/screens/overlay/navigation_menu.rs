use crate::catalog::NavSection;
use functora_egui::snippet;
use functora_egui::{NavigationMenuValue, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_navigation_menu(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Top-level navigation with active item tracking.").show(ui);
        ui.add_space(12.0);
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

        snippet(
            ui,
            "// NavigationMenuValue: top-level navigation bound to an enum\nuse functora_egui::NavigationMenuValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum NavSection { Overview, Integrations, Settings }\n\nlet entries = [(NavSection::Overview, \"Overview\".to_owned()), (NavSection::Integrations, \"Integrations\".to_owned()), (NavSection::Settings, \"Settings\".to_owned())];\nlet mut active = NavSection::Overview;\n\nif let Some(section) = NavigationMenuValue::new(&entries).show(ui, &mut active) {\n    eprintln!(\"Navigated to: {section:?}\");\n}",
        );
    }
}
