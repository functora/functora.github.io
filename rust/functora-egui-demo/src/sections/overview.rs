//! Overview section: the library pitch and feature highlights.

use crate::app::{CATEGORIES, CategoryId, OverviewBody};
use functora_egui::i18n::I18N;
use functora_egui::{Flex, Separator, Typography};

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_overview(&mut self, ui: &mut egui::Ui, lang: functora_egui::i18n::Language) {
        _ = Typography::new(OverviewBody.render(lang)).show(ui);
        ui.add_space(12.0);

        for (cat_id, _, items) in CATEGORIES
            .iter()
            .filter(|(id, _, _)| *id != CategoryId::Overview)
        {
            _ = Separator::horizontal()
                .text(cat_id.render(lang))
                .icon(cat_id.icon())
                .show(ui);
            ui.add_space(8.0);
            let ctx = ui.ctx().clone();
            let current = self.selected;
            _ = Flex::row().gap(4.0).wrap().show(ui, |f| {
                for def in *items {
                    let Some(id) = def.id else {
                        continue;
                    };
                    let selected = Some(id) == current;
                    if f.add(crate::app::section_button(def, selected))
                        .inner
                        .clicked()
                    {
                        self.navigate_to(Some(id));
                        ctx.request_repaint();
                    }
                }
            });
            ui.add_space(8.0);
        }
    }
}
