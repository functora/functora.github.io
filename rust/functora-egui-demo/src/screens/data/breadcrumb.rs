use functora_egui::snippet;
use functora_egui::{Breadcrumb, NavAction, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_breadcrumb(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Navigation trail using generic Breadcrumb with NavHistory.")
            .show(ui);
        ui.add_space(12.0);
        let lang = functora_egui::i18n::detect_browser_language();
        if let Some(action) =
            Breadcrumb::new(self.router.current(), self.router.history()).show(ui, lang)
        {
            match action {
                NavAction::Back => {
                    _ = self.router.go_back();
                }
                NavAction::Forward => {
                    _ = self.router.go_forward();
                }
                NavAction::Route(route) => {
                    self.navigate_to(route.component());
                }
            }
        }
        ui.add_space(12.0);
        _ = Typography::small("Custom separator (uses router history)").show(ui);
        ui.add_space(4.0);
        if let Some(action) = Breadcrumb::new(self.router.current(), self.router.history())
            .separator(" > ")
            .show(ui, lang)
        {
            match action {
                NavAction::Back => {
                    _ = self.router.go_back();
                }
                NavAction::Forward => {
                    _ = self.router.go_forward();
                }
                NavAction::Route(route) => {
                    self.navigate_to(route.component());
                }
            }
        }

        snippet(
            ui,
            "// Generic Breadcrumb with NavHistory\nuse functora_egui::{Breadcrumb, NavAction};\n\nlet lang = functora_egui::i18n::detect_browser_language();\nif let Some(action) = Breadcrumb::new(router.current(), router.history())\n    .show(ui, lang)\n{\n    match action {\n        NavAction::Back => router.go_back(),\n        NavAction::Forward => router.go_forward(),\n        NavAction::Route(route) => router.navigate(route),\n    }\n}\n\n// Custom separator (uses router history)\nif let Some(action) = Breadcrumb::new(router.current(), router.history())\n    .separator(\" > \")\n    .show(ui, lang)\n{\n    match action {\n        NavAction::Back => router.go_back(),\n        NavAction::Forward => router.go_forward(),\n        NavAction::Route(route) => router.navigate(route),\n    }\n}",
        );
    }
}
