use crate::icons::lucide_icon::LucideIcon;
use crate::responsive::responsive_ext::ResponsiveExt;
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
use crate::theme_extra::Theme;
use crate::tokens::button_variant::ButtonVariant;
use crate::tokens::component_size::ComponentSize;
use crate::widgets::button::widget::Button;

impl super::widget::Navbar<'_> {
    pub fn show(
        self,
        outer_ui: &mut egui::Ui,
        collapsed: &mut bool,
        theme: Option<&mut Theme>,
        language: Option<&mut functora_core::i18n::Language>,
        mut on_brand: Option<&mut dyn FnMut()>,
        mut on_search: Option<&mut dyn FnMut()>,
    ) -> egui::Response {
        let mut theme_opt = theme;
        let mut lang_opt = language;
        let frame = egui::Frame::NONE.inner_margin(egui::Margin {
            left: 8,
            right: 8,
            top: 6,
            bottom: 6,
        });
        frame
            .show(outer_ui, |frame_ui| {
                let _ = crate::layout::flex::Flex::row()
                    .gap(4.0)
                    .justify_between()
                    .align_center()
                    .w_full()
                    .show(frame_ui, |flex| {
                        let _ = flex.ui(|left_ui| {
                            let _ = left_ui.horizontal(|inner_ui| {
                                let ctx = inner_ui.ctx().clone();
                                let th = ShadcnThemeExt::shadcn_theme(&ctx);
                                let brand_resp = inner_ui
                                    .add(
                                        egui::Label::new(
                                            egui::RichText::new(self.brand)
                                                .size(20.0)
                                                .strong()
                                                .color(th.foreground),
                                        )
                                        .selectable(false)
                                        .sense(egui::Sense::click()),
                                    )
                                    .on_hover_cursor(egui::CursorIcon::PointingHand);
                                if brand_resp.clicked()
                                    && let Some(cb) = on_brand.as_mut()
                                {
                                    cb();
                                }
                                if let Some(vsn) = self.version {
                                    inner_ui.add_space(8.0);
                                    let _ = inner_ui.label(
                                        egui::RichText::new(format!("v{vsn}"))
                                            .size(10.0)
                                            .color(th.muted_foreground),
                                    );
                                }
                            });
                        });
                        let _ = flex.ui(|right_ui| {
                            let _ = right_ui.horizontal(|inner_ui| {
                                if let Some(lang) = lang_opt.take() {
                                    let current =
                                        lang.to_639_1().unwrap_or("en").to_ascii_uppercase();
                                    let trigger = inner_ui.add(
                                        Button::new(current)
                                            .icon(LucideIcon::Languages)
                                            .variant(ButtonVariant::Ghost)
                                            .size(ComponentSize::Sm),
                                    );
                                    let items: Vec<
                                        crate::widgets::dropdown_menu::widget::MenuItem,
                                    > = crate::i18n::SUPPORTED_LANGUAGES
                                        .iter()
                                        .map(|l| {
                                            let code =
                                                l.to_639_1().unwrap_or("en").to_ascii_uppercase();
                                            let selected = l == lang;
                                            crate::widgets::dropdown_menu::widget::MenuItem::label(
                                                code,
                                            )
                                            .selected(selected)
                                        })
                                        .collect();
                                    let mut chosen: Option<usize> = None;
                                    crate::widgets::dropdown_menu::widget::DropdownMenu::show_rich(
                                        inner_ui,
                                        &trigger,
                                        &items,
                                        |idx| chosen = Some(idx),
                                    );
                                    if let Some(idx) = chosen
                                        && let Some(new_lang) =
                                            crate::i18n::SUPPORTED_LANGUAGES.get(idx)
                                    {
                                        *lang = *new_lang;
                                    }
                                    inner_ui.add_space(4.0);
                                }
                                if let Some(label) = self.search_label {
                                    let search = if inner_ui.on_mobile() {
                                        Button::icon_only(LucideIcon::Search)
                                            .variant(ButtonVariant::Outline)
                                            .size(ComponentSize::Sm)
                                    } else {
                                        let mut b = Button::new(label)
                                            .icon(LucideIcon::Search)
                                            .variant(ButtonVariant::Outline)
                                            .size(ComponentSize::Sm);
                                        if let Some(sc) = self.search_shortcut {
                                            b = b.shortcut_text(sc);
                                        }
                                        b
                                    };
                                    if inner_ui.add(search).clicked()
                                        && let Some(cb) = on_search.as_mut()
                                    {
                                        cb();
                                    }
                                    inner_ui.add_space(4.0);
                                }
                                if let Some(th) = theme_opt.take() {
                                    let icon = if *th == Theme::Dark {
                                        LucideIcon::Moon
                                    } else {
                                        LucideIcon::Sun
                                    };
                                    let hover = if *th == Theme::Dark {
                                        "Light theme"
                                    } else {
                                        "Dark theme"
                                    };
                                    if inner_ui
                                        .add(
                                            Button::icon_only(icon)
                                                .variant(ButtonVariant::Outline)
                                                .size(ComponentSize::Sm),
                                        )
                                        .on_hover_text(hover)
                                        .clicked()
                                    {
                                        *th = th.next();
                                        crate::theme_extra::set_theme(inner_ui.ctx(), *th);
                                    }
                                    inner_ui.add_space(4.0);
                                }
                                let _ = crate::widgets::sidebar::widget::Sidebar::toggle_button(
                                    inner_ui, collapsed,
                                );
                            });
                        });
                    });
            })
            .response
    }
}
