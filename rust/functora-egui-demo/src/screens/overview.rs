use crate::catalog::{
    CATEGORIES, CategoryId, LIBRARY_CRATES_URL, LIBRARY_SOURCE_URL, OverviewBody,
};
use crate::route::AppRoute;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Flex, LucideIcon, Separator, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_overview(&mut self, ui: &mut egui::Ui, lang: functora_egui::i18n::Language) {
        _ = Typography::new(OverviewBody.render(lang)).show(ui);
        ui.add_space(8.0);
        let action_ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(
                Button::new(BaseMsg::SourceCodeButton.render(lang))
                    .icon(LucideIcon::Github)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                action_ctx.open_url(egui::OpenUrl::new_tab(LIBRARY_SOURCE_URL));
            }
            if f.add(
                Button::new("crates.io")
                    .icon(LucideIcon::Package)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                action_ctx.open_url(egui::OpenUrl::new_tab(LIBRARY_CRATES_URL));
            }
            if f.add(
                Button::new(BaseMsg::Donate.render(lang))
                    .icon(LucideIcon::Heart)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                self.navigate_route(AppRoute::Donate);
                action_ctx.request_repaint();
            }
        });
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
                    if f.add(crate::catalog::section_button(def, selected))
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
