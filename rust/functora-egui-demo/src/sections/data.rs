//! Data: avatars, breadcrumbs, calendars, carousels, pagination, sidebars,
//! tables, and area charts.

use functora_egui::{
    AreaChart, AreaSeries, Avatar, Badge, Breadcrumb, Button, ButtonVariant, Calendar, Carousel,
    Flex, LucideIcon, Pagination, ResponsiveExt, Separator, Sidebar, Table, Typography,
};

use super::code::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_avatar(ui: &mut egui::Ui) {
        _ = Typography::muted("Initials-based avatars with adjustable sizes.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(16.0).align_center().show(ui, |f| {
            _ = f.add(Avatar::new("AL").size(24.0));
            _ = f.add(Avatar::new("CM").size(32.0));
            _ = f.add(Avatar::new("DA").size(40.0));
            _ = f.add(Avatar::new("FN").size(56.0));
        });
        ui.add_space(8.0);
        _ = Typography::small("Colors come from the theme's primary palette.").show(ui);

        snippet(
            ui,
            "Avatar::new(\"AL\").size(24.0).show(ui);\nAvatar::new(\"CM\").size(32.0).show(ui);",
        );
    }

    pub(crate) fn demo_breadcrumb(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Navigation trail of the current location.").show(ui);
        ui.add_space(12.0);
        let items = vec![
            "Home".to_owned(),
            "Components".to_owned(),
            "Data Display".to_owned(),
        ];
        let clicked = Breadcrumb::new(items).show(ui);
        if let Some(idx) = clicked {
            self.toast.add(
                format!("Breadcrumb: item {idx}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(12.0);
        _ = Typography::small("Custom separator").show(ui);
        ui.add_space(4.0);
        _ = Breadcrumb::new(vec!["a".to_owned(), "b".to_owned(), "c".to_owned()])
            .separator("/")
            .show(ui);

        snippet(
            ui,
            "let clicked = Breadcrumb::new(items).show(ui);\nBreadcrumb::new(items).separator(\"/\").show(ui);",
        );
    }

    pub(crate) fn demo_calendar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A month grid calendar with navigation.").show(ui);
        ui.add_space(12.0);
        let clicked = Calendar::new().show(
            ui,
            &mut self.calendar_year,
            &mut self.calendar_month,
            &mut self.calendar_day,
        );
        if let Some(day) = clicked {
            self.calendar_day = day;
            self.toast.add(
                format!("Selected {day}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "{:04}-{:02}-{:02}",
            self.calendar_year, self.calendar_month, self.calendar_day
        ))
        .show(ui);

        snippet(
            ui,
            "let clicked = Calendar::new().show(ui, &mut year, &mut month, &mut day);",
        );
    }

    pub(crate) fn demo_carousel(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A slider with prev/next navigation and dots.").show(ui);
        ui.add_space(12.0);
        let colors = [
            egui::Color32::from_rgb(25, 113, 194),
            egui::Color32::from_rgb(18, 184, 134),
            egui::Color32::from_rgb(245, 159, 0),
            egui::Color32::from_rgb(224, 49, 49),
        ];
        _ = Carousel::new(colors.len()).show(ui, &mut self.carousel_idx, |ui50, idx| {
            _ = ui50.allocate_ui(
                egui::vec2(ui50.available_width().min(420.0), 200.0),
                |ui15| {
                    let rect = ui15.available_rect_before_wrap();
                    let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui15.ctx());
                    _ = ui15.painter().rect_filled(
                        rect,
                        egui::CornerRadius::from(theme.radius),
                        colors[idx],
                    );
                    let galley = ui15.painter().layout_no_wrap(
                        format!("Slide {}", idx + 1),
                        egui::FontId::proportional(24.0),
                        egui::Color32::WHITE,
                    );
                    ui15.painter().galley(
                        egui::pos2(
                            rect.center().x - galley.size().x / 2.0,
                            rect.center().y - galley.size().y / 2.0,
                        ),
                        galley,
                        egui::Color32::WHITE,
                    );
                },
            );
        });

        snippet(
            ui,
            "Carousel::new(count).show(ui, &mut index, |slide, idx| { ... });",
        );
    }

    pub(crate) fn demo_pagination(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Page navigation with visible range window.").show(ui);
        ui.add_space(12.0);
        let max_vis = if ui.on_mobile() { 5 } else { 7 };
        _ = Pagination::new(20)
            .max_visible(max_vis)
            .show(ui, &mut self.pagination_page);
        ui.add_space(4.0);
        _ = Typography::small(format!("Page {} of 20", self.pagination_page + 1)).show(ui);

        snippet(
            ui,
            "Pagination::new(20).max_visible(7).show(ui, &mut page);",
        );
    }

    pub(crate) fn demo_sidebar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "A collapsible navigation panel, responsive on mobile (slides in as a drawer).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Sidebar::new().width(228.0).collapsible().show(
            ui,
            &mut self.nav.sidebar_demo_collapsed,
            |ui16| {
                _ = Typography::small("Navigation")
                    .variant(functora_egui::TypographyVariant::Muted)
                    .show(ui16);
                ui16.add_space(4.0);
                if Button::new("Overview")
                    .icon(LucideIcon::Sparkles)
                    .variant(ButtonVariant::Ghost)
                    .full_width()
                    .show(ui16)
                    .clicked()
                {
                    self.toast.add(
                        "Overview",
                        functora_egui::ToastVariant::Default,
                        ui16.ctx().input(|i| i.time),
                    );
                }
                if Button::new("Settings")
                    .icon(LucideIcon::Settings)
                    .variant(ButtonVariant::Ghost)
                    .full_width()
                    .show(ui16)
                    .clicked()
                {
                    self.toast.add(
                        "Settings",
                        functora_egui::ToastVariant::Default,
                        ui16.ctx().input(|i| i.time),
                    );
                }
            },
        );

        snippet(
            ui,
            "Sidebar::new().width(228.0).collapsible().show(ui, &mut collapsed, |nav| { ... });",
        );
    }

    pub(crate) fn demo_table(ui: &mut egui::Ui) {
        _ = Typography::muted("A styled table with headers, rows, and optional striping.").show(ui);
        ui.add_space(12.0);
        let headers = vec!["Name".to_owned(), "Status".to_owned(), "Role".to_owned()];
        let rows = vec![
            vec![
                "Ada Lovelace".to_owned(),
                "Active".to_owned(),
                "Admin".to_owned(),
            ],
            vec![
                "Alan Turing".to_owned(),
                "Active".to_owned(),
                "Editor".to_owned(),
            ],
            vec![
                "Grace Hopper".to_owned(),
                "Inactive".to_owned(),
                "Viewer".to_owned(),
            ],
            vec![
                "Edsger Dijkstra".to_owned(),
                "Active".to_owned(),
                "Editor".to_owned(),
            ],
        ];
        _ = Table::new(headers).rows(rows).striped().show(ui);
        ui.add_space(12.0);
        _ = Typography::small("With custom column weights").show(ui);
        ui.add_space(4.0);
        _ = Table::new(vec![
            "Name".to_owned(),
            "Email".to_owned(),
            "Role".to_owned(),
        ])
        .rows(vec![
            vec![
                "Ada Lovelace".to_owned(),
                "ada@example.com".to_owned(),
                "Admin".to_owned(),
            ],
            vec![
                "Alan Turing".to_owned(),
                "alan@example.com".to_owned(),
                "Editor".to_owned(),
            ],
        ])
        .col_weights(vec![0.4, 0.4, 0.2])
        .show(ui);

        snippet(
            ui,
            "Table::new(headers).rows(rows).striped().show(ui);\nTable::new(headers).rows(rows).col_weights(vec![0.4, 0.4, 0.2]).show(ui);",
        );
    }

    pub(crate) fn demo_area_chart(ui: &mut egui::Ui) {
        _ = Typography::muted("A stacked area chart with smooth curves.").show(ui);
        ui.add_space(12.0);
        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let series_alpha = egui::Color32::from_rgba_unmultiplied(
            theme.primary.r(),
            theme.primary.g(),
            theme.primary.b(),
            160,
        );
        let series_beta = egui::Color32::from_rgba_unmultiplied(
            theme.secondary.r(),
            theme.secondary.g(),
            theme.secondary.b(),
            180,
        );
        _ = AreaChart::new(vec![
            "Jan".to_owned(),
            "Feb".to_owned(),
            "Mar".to_owned(),
            "Apr".to_owned(),
            "May".to_owned(),
            "Jun".to_owned(),
        ])
        .series(AreaSeries {
            values: vec![186.0, 305.0, 237.0, 73.0, 209.0, 214.0],
            color: series_alpha,
        })
        .series(AreaSeries {
            values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],
            color: series_beta,
        })
        .stacked()
        .height(260.0)
        .show(ui);
        ui.add_space(12.0);
        _ = Separator::horizontal().show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Badge::new("Primary"));
            _ = f.add(Badge::new("Secondary"));
        });

        snippet(
            ui,
            "AreaChart::new(months)\n    .series(AreaSeries { values, color })\n    .stacked()\n    .height(260.0)\n    .show(ui);",
        );
    }
}
