//! Overview section: the library pitch and feature highlights.

use crate::app::CATEGORIES;
use functora_egui::{
    Button, ButtonVariant, Card, ComponentSize, Flex, LucideIcon, Separator, Typography,
};

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_overview(&mut self, ui: &mut egui::Ui) {
        _ = Typography::lead(
            "shadcn/ui-inspired widgets for egui: 60+ styled components with light and dark \
             theming, responsive mobile-first behavior, and 1600+ Lucide icons.",
        )
        .show(ui);
        ui.add_space(12.0);

        _ = Card::new().show(ui, |ui70| {
            _ = Typography::h4("What is this app?").show(ui70);
            ui70.add_space(4.0);
            _ = Typography::new(
                "This page is an interactive showcase of every functora-egui layout, component, \
                 widget, and feature. Pick any entry in the sidebar (or press Ctrl+K and search) \
                 to explore a live demo. Resize the window below 800px to see the responsive \
                 mobile behavior: the sidebar becomes a slide-in drawer, dialogs become bottom \
                 sheets, and controls grow to touch-friendly sizes.",
            )
            .show(ui70);
            ui70.add_space(8.0);
            let ctx = ui70.ctx().clone();
            _ = Flex::row().gap(8.0).wrap().show(ui70, |f| {
                if f.add(
                    Button::new("Components")
                        .icon(LucideIcon::Component)
                        .variant(ButtonVariant::Outline)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                    && let Some(idx) = crate::app::component_index("Button")
                {
                    self.navigate_to(idx);
                    ctx.request_repaint();
                }
                if f.add(
                    Button::new("Themes")
                        .icon(LucideIcon::Palette)
                        .variant(ButtonVariant::Outline)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                    && let Some(idx) = crate::app::component_index("Typography")
                {
                    self.navigate_to(idx);
                    ctx.request_repaint();
                }
                if f.add(
                    Button::new("Responsive")
                        .icon(LucideIcon::MonitorSmartphone)
                        .variant(ButtonVariant::Outline)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                    && let Some(idx) = crate::app::component_index("Breakpoint")
                {
                    self.navigate_to(idx);
                    ctx.request_repaint();
                }
            });
        });

        ui.add_space(16.0);

        for (cat_idx, (cat_name, cat_icon, items)) in CATEGORIES
            .iter()
            .enumerate()
            .filter(|(_, (name, _, _))| *name != "Overview")
        {
            _ = Separator::horizontal()
                .text(*cat_name)
                .icon(*cat_icon)
                .show(ui);
            ui.add_space(8.0);
            let ctx = ui.ctx().clone();
            _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
                for (item_idx, def) in items.iter().enumerate() {
                    let flat = crate::app::flat_index(cat_idx, item_idx);
                    let selected = flat == self.selected;
                    if f.add(crate::app::section_button(def, selected))
                        .inner
                        .clicked()
                    {
                        self.navigate_to(flat);
                        ctx.request_repaint();
                    }
                }
            });
            ui.add_space(8.0);
        }
    }
}
