//! Data: avatars, breadcrumbs, calendars, carousels, pagination, sidebars,
//! tables, and area charts.

use functora_egui::{
    AreaChart, AreaSeries, Avatar, Badge, Breadcrumb, Button, ButtonVariant, Calendar, Carousel,
    Flex, LucideIcon, NavAction, Pagination, ResponsiveExt, Separator, Sidebar, Table, Typography,
};

use functora_egui::snippet;

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
            "// Avatar: initials-based avatar with adjustable sizes\nuse functora_egui::{Avatar, Flex};\n\nFlex::row().gap(16.0).align_center().show(ui, |f| {\n    f.add(Avatar::new(\"AL\").size(24.0));\n    f.add(Avatar::new(\"CM\").size(32.0));\n    f.add(Avatar::new(\"DA\").size(40.0));\n    f.add(Avatar::new(\"FN\").size(56.0));\n});",
        );
    }

    pub(crate) fn demo_breadcrumb(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Navigation trail using generic Breadcrumb with NavHistory.")
            .show(ui);
        ui.add_space(12.0);
        let lang = functora_egui::i18n::detect_browser_language();
        if let Some(action) =
            Breadcrumb::new(self.router.current(), self.router.history()).show(ui, lang)
        {
            match action {
                NavAction::Back => {
                    _ = self.router.go_back(&mut ());
                }
                NavAction::Forward => {
                    _ = self.router.go_forward(&mut ());
                }
                NavAction::Route(route) => {
                    if let Some(idx) = route.to_flat() {
                        self.navigate_to(idx);
                    }
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
                    _ = self.router.go_back(&mut ());
                }
                NavAction::Forward => {
                    _ = self.router.go_forward(&mut ());
                }
                NavAction::Route(route) => {
                    if let Some(idx) = route.to_flat() {
                        self.navigate_to(idx);
                    }
                }
            }
        }

        snippet(
            ui,
            "// Generic Breadcrumb with NavHistory\nuse functora_egui::{Breadcrumb, NavAction};\n\nlet lang = functora_egui::i18n::detect_browser_language();\nif let Some(action) = Breadcrumb::new(router.current(), router.history())\n    .show(ui, lang)\n{\n    match action {\n        NavAction::Back => router.go_back(&mut ()),\n        NavAction::Forward => router.go_forward(&mut ()),\n        NavAction::Route(route) => router.navigate(&mut (), route),\n    }\n}",
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
            "// Calendar: month grid with navigation\nuse functora_egui::Calendar;\n\nlet mut year = 2026;\nlet mut month = 8;\nlet mut day = 20;\n\nif let Some(clicked_day) = Calendar::new().show(ui, &mut year, &mut month, &mut day) {\n    day = clicked_day;\n    eprintln!(\"Selected: {:04}-{:02}-{:02}\", year, month, day);\n}",
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
            "// Carousel: slider with prev/next + dots\nuse functora_egui::Carousel;\n\nlet items = [\"Slide 1\", \"Slide 2\", \"Slide 3\", \"Slide 4\"];\nlet mut index = 0;\n\nCarousel::new(items.len()).show(ui, &mut index, |slide, idx| {\n    slide.allocate_ui(egui::vec2(slide.available_width().min(420.0), 200.0), |ui| {\n        let rect = ui.available_rect_before_wrap();\n        ui.painter().rect_filled(rect, theme.radius, colors[idx]);\n        ui.painter().galley(\n            rect.center() - galley.size() / 2.0,\n            galley,\n            Color32::WHITE,\n        );\n    });\n});",
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
            "// Pagination: page navigation with visible range\nuse functora_egui::Pagination;\n\nlet mut page = 0;\nPagination::new(20)\n    .max_visible(7)\n    .show(ui, &mut page);\n\neprintln!(\"Page {} of 20\", page + 1);",
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
            "// Sidebar: collapsible navigation panel (responsive drawer on mobile)\nuse functora_egui::{Sidebar, Button, ButtonVariant, LucideIcon, Typography, TypographyVariant};\n\nlet mut collapsed = false;\nSidebar::new()\n    .width(228.0)\n    .collapsible()\n    .show(ui, &mut collapsed, |nav| {\n        Typography::small(\"Navigation\").variant(TypographyVariant::Muted).show(nav);\n        nav.add_space(4.0);\n        Button::new(\"Overview\").icon(LucideIcon::Sparkles).variant(ButtonVariant::Ghost).full_width().show(nav);\n        Button::new(\"Settings\").icon(LucideIcon::Settings).variant(ButtonVariant::Ghost).full_width().show(nav);\n    });",
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
            "// Table: styled table with headers, rows, striping, column weights\nuse functora_egui::Table;\n\nlet headers = vec![\"Name\", \"Status\", \"Role\"];\nlet rows = vec![\n    vec![\"Ada Lovelace\", \"Active\", \"Admin\"],\n    vec![\"Alan Turing\", \"Active\", \"Editor\"],\n    vec![\"Grace Hopper\", \"Inactive\", \"Viewer\"],\n];\n\n// Basic with striping\nTable::new(headers.clone()).rows(rows.clone()).striped().show(ui);\n\n// Custom column weights\nTable::new(vec![\"Name\", \"Email\", \"Role\"])\n    .rows(vec![\n        vec![\"Ada Lovelace\", \"ada@example.com\", \"Admin\"],\n        vec![\"Alan Turing\", \"alan@example.com\", \"Editor\"],\n    ])\n    .col_weights(vec![0.4, 0.4, 0.2])\n    .show(ui);",
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
            "// AreaChart: stacked area chart with smooth curves\nuse functora_egui::{AreaChart, AreaSeries};\nuse egui::Color32;\n\nlet months = vec![\"Jan\", \"Feb\", \"Mar\", \"Apr\", \"May\", \"Jun\"];\n\nlet primary = Color32::from_rgba_unmultiplied(25, 113, 194, 160);\nlet secondary = Color32::from_rgba_unmultiplied(18, 184, 134, 180);\n\nAreaChart::new(months)\n    .series(AreaSeries {\n        values: vec![186.0, 305.0, 237.0, 73.0, 209.0, 214.0],\n        color: primary,\n    })\n    .series(AreaSeries {\n        values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],\n        color: secondary,\n    })\n    .stacked()\n    .height(260.0)\n    .show(ui);",
        );
    }
}
