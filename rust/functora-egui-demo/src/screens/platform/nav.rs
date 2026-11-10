use crate::route::AppRoute;
use functora_egui::Routable;
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Standalone NavHistory");
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Push Button").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.nav_history
                    .push(AppRoute::Component(crate::catalog::ComponentId::Button));
            }
            if f.add(Button::new("Replace with Input").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.nav_history
                    .replace(AppRoute::Component(crate::catalog::ComponentId::Input));
            }
            if f.add(Button::new("Truncate forward").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.nav_history.truncate_forward();
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "History current: {}, can_go_back: {}, can_go_forward: {}",
            self.nav_history.current(),
            self.nav_history.can_go_back(),
            self.nav_history.can_go_forward()
        ))
        .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Breadcrumbs");
        let crumbs =
            functora_egui::breadcrumbs_for(self.router.current(), self.persistent.language);
        _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
            for crumb in &crumbs {
                _ = f.ui(|ui2| {
                    _ = Typography::small(format!("{} ({:?})", crumb.name, crumb.position))
                        .show(ui2);
                });
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "last crumb is_last(): {}",
            crumbs
                .last()
                .is_none_or(functora_egui::BreadcrumbSegment::is_last)
        ))
        .show(ui);
        _ = Typography::small(format!(
            "route params: screen = {:?}, component = {:?}, root = {:?}",
            functora_egui::SCREEN_PARAM,
            functora_egui::COMPONENT_PARAM,
            functora_egui::ROOT_PATH
        ))
        .show(ui);
        _ = Typography::small(format!(
            "slug: {:?}, url: {:?}, from_url: {:?}",
            self.router.current().to_slug(),
            self.router.current().to_url(),
            AppRoute::from_url(self.router.current().to_url().as_str())
        ))
        .show(ui);
        _ = Typography::small(format!(
            "resolve_history_url(\"/app\", \"{:?}\"): {:?}",
            self.router.current().to_url(),
            functora_egui::route::resolve_history_url(
                "/app",
                self.router.current().to_url().as_str()
            )
        ))
        .show(ui);
        _ = Typography::small(format!(
            "BreadcrumbPosition: {:?}, {:?}",
            functora_egui::BreadcrumbPosition::Last,
            functora_egui::BreadcrumbPosition::NotLast
        ))
        .show(ui);
        _ = Typography::small(
            "browser history helpers: history_push / history_replace are exported at the crate root",
        )
        .show(ui);

        ui.add_space(8.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Example: NavHistory + AppRouter").show(ui2);
            ui2.add_space(4.0);
            snippet(
                ui2,
                "// NavHistory: push / go_back / go_forward / sync\nuse functora_egui::nav::NavHistory;\nuse functora_egui::route::AppRouter;\n\n// AppRoute + ComponentId are your own route types\nlet mut history = NavHistory::new(AppRoute::Overview);\n\n// Push a route\nhistory.push(AppRoute::Component(ComponentId::Button));\nassert_eq!(history.current(), &AppRoute::Component(ComponentId::Button));\n\n// Go back\nhistory.go_back();\nassert_eq!(history.current(), &AppRoute::Overview);\n\n// Check state\nhistory.can_go_back(); // false\nhistory.can_go_forward(); // true\n\n// AppRouter integrates with browser history\nlet mut router = AppRouter::new(&AppRoute::Overview);\nrouter.navigate(AppRoute::Component(ComponentId::Button));\nrouter.go_back();\n\n// Replace the current entry instead of pushing a new one\nhistory.replace(AppRoute::Component(ComponentId::Input));\nassert_eq!(history.current(), &AppRoute::Component(ComponentId::Input));\n\n// Drop everything that was ahead of the current entry\nhistory.truncate_forward();\nassert!(!history.can_go_forward());\n\n// Browser history can also be driven directly\nuse functora_egui::{history_push, history_replace};\n\nhistory_push(&AppRoute::Component(ComponentId::Button));\nhistory_replace(&AppRoute::Component(ComponentId::Input));\n\n// Breadcrumbs carry their position in the trail\nuse functora_egui::{BreadcrumbPosition, breadcrumbs_for};\n\nlet crumbs = breadcrumbs_for(&AppRoute::Component(ComponentId::Button), Language::En);\nfor crumb in &crumbs {\n    match crumb.position {\n        BreadcrumbPosition::Last => { /* the current page */ }\n        BreadcrumbPosition::NotLast => { /* an ancestor */ }\n    }\n}",
            );
        });
    }
}
