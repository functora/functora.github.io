//! Overview section: the library pitch and feature highlights.

use crate::app::{CATEGORIES, CategoryId};
use functora_egui::{Flex, Separator, Typography};

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_overview(&mut self, ui: &mut egui::Ui) {
        _ = Typography::new(
            "Interactive showcase of 60+ shadcn/ui-inspired widgets for egui with light/dark \
             themes and 1600+ Lucide icons. Browse via the sidebar or press Ctrl+K.",
        )
        .show(ui);
        ui.add_space(16.0);

        for (cat_idx, (cat_id, _, items)) in CATEGORIES
            .iter()
            .enumerate()
            .filter(|(_, (id, _, _))| *id != CategoryId::Overview)
        {
            _ = Separator::horizontal()
                .text(cat_id.label())
                .icon(cat_id.icon())
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
