use crate::route::AppRoute;
use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_nav(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "NavHistory<R> + AppRouter<R, S>: push/go_back/go_forward, integrates with browser history."
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!("Current route: {}", self.router.current())).show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Go back").icon(functora_egui::LucideIcon::ArrowLeft))
                .inner
                .clicked()
            {
                _ = self.router.go_back();
            }
            if f.add(Button::new("Go forward").icon(functora_egui::LucideIcon::ArrowRight))
                .inner
                .clicked()
            {
                _ = self.router.go_forward();
            }
            if f.add(Button::new("Navigate to Overview").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.router.navigate(AppRoute::Overview);
            }
        });
        ui.add_space(8.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Example: NavHistory + AppRouter").show(ui2);
            ui2.add_space(4.0);
            snippet(
                ui2,
                "// NavHistory: push / go_back / go_forward / sync\nuse functora_egui::nav::NavHistory;\nuse functora_egui::route::AppRouter;\n\n// AppRoute + ComponentId are your own route types\nlet mut history = NavHistory::new(AppRoute::Overview);\n\n// Push a route\nhistory.push(AppRoute::Component(ComponentId::Button));\nassert_eq!(history.current(), &AppRoute::Component(ComponentId::Button));\n\n// Go back\nhistory.go_back();\nassert_eq!(history.current(), &AppRoute::Overview);\n\n// Check state\nhistory.can_go_back(); // false\nhistory.can_go_forward(); // true\n\n// AppRouter integrates with browser history\nlet mut router = AppRouter::new(&AppRoute::Overview);\nrouter.navigate(AppRoute::Component(ComponentId::Button));\nrouter.go_back();",
            );
        });
    }
}
