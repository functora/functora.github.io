//! Data: avatars, breadcrumbs, calendars, carousels, pagination, sidebars,
//! tables, and area charts.

use functora_egui::{
    AreaChart, AreaSeries, Avatar, Badge, BadgeVariant, Breadcrumb, Calendar, Carousel, Flex,
    NavAction, Pagination, ResponsiveExt, Separator, Table, Typography,
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
        let total = colors.len();
        _ = Carousel::new(total).show(ui, &mut self.carousel_idx, |slide_ui, idx| {
            let width = slide_ui.available_width().min(420.0);
            let (rect, _) =
                slide_ui.allocate_exact_size(egui::vec2(width, 200.0), egui::Sense::hover());
            if slide_ui.is_rect_visible(rect) {
                let theme = functora_egui::ShadcnThemeExt::shadcn_theme(slide_ui.ctx());
                let painter = slide_ui.painter();
                _ = painter.rect_filled(rect, egui::CornerRadius::from(theme.radius), colors[idx]);
                let galley = painter.layout_no_wrap(
                    format!("Slide {}", idx + 1),
                    egui::FontId::proportional(24.0),
                    egui::Color32::WHITE,
                );
                painter.galley(
                    egui::pos2(
                        rect.center().x - galley.size().x / 2.0,
                        rect.center().y - galley.size().y / 2.0,
                    ),
                    galley,
                    egui::Color32::WHITE,
                );
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!("Slide {} of {total}", self.carousel_idx + 1)).show(ui);

        snippet(
            ui,
            "// Carousel: slider with prev/next + dots\nuse functora_egui::Carousel;\n\nlet items = [\"Slide 1\", \"Slide 2\", \"Slide 3\", \"Slide 4\"];\nlet mut index = 0;\n\nCarousel::new(items.len()).show(ui, &mut index, |slide, idx| {\n    let width = slide.available_width().min(420.0);\n    let (rect, _) = slide.allocate_exact_size(egui::vec2(width, 200.0), egui::Sense::hover());\n    slide.painter().rect_filled(rect, theme.radius, colors[idx]);\n    slide.painter().galley(rect.center() - galley.size() / 2.0, galley, Color32::WHITE);\n});",
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
            "// Pagination: page navigation with visible range\nuse functora_egui::Pagination;\n\nlet mut page = 0;\nlet max_visible = if ui.on_mobile() { 5 } else { 7 };\nPagination::new(20)\n    .max_visible(max_visible)\n    .show(ui, &mut page);\n\neprintln!(\"Page {} of 20\", page + 1);",
        );
    }

    pub(crate) fn demo_sidebar(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "The app sidebar is the navigation panel on the side. This page documents it, so there is no second live demo here.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(
            "On desktop it is a side panel, on mobile it slides in as a drawer toggled by the header hamburger button. Resize below 800px: the sidebar covers the screen as a drawer.",
        )
        .show(ui);

        snippet(
            ui,
            "// Sidebar: the app shell owns the only live sidebar, so this page is snippet-only\nuse functora_egui::{ResponsiveExt, Shell};\n\nlet mut collapsed = ui.on_mobile();\nShell::new(\"functora-egui\", &mut collapsed, |side| {\n    for (cat_idx, (cat_id, _, items)) in CATEGORIES.iter().enumerate() {\n        category_header(side, *cat_id, lang);\n        side.add_space(8.0);\n        for (item_idx, def) in items.iter().enumerate() {\n            let selected = flat_index(cat_idx, item_idx) == selected_flat;\n            if side.add(section_button(def, selected).full_width()).clicked() {\n                selected_flat = flat_index(cat_idx, item_idx);\n            }\n        }\n        side.add_space(8.0);\n    }\n    false\n})\n.theme(&mut theme)\n.search(\"Search\", Some(\"Ctrl K\"))\n.breadcrumb(route, history)\n.show(ui, |content| {\n    // page content, no second Sidebar here\n});",
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

    pub fn demo_area_chart(ui: &mut egui::Ui) {
        _ = Typography::muted("A stacked area chart with smooth curves.").show(ui);
        ui.add_space(12.0);
        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
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
            color: theme.chart_1,
        })
        .series(AreaSeries {
            values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],
            color: theme.chart_2,
        })
        .stacked()
        .height(260.0)
        .show(ui);
        ui.add_space(12.0);
        _ = Separator::horizontal().show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Badge::new("Alpha"));
            _ = f.add(Badge::new("Beta").variant(BadgeVariant::Success));
        });

        snippet(
            ui,
            "// AreaChart: stacked area chart with smooth curves\nuse functora_egui::{AreaChart, AreaSeries, Badge, BadgeVariant};\n\nlet theme = ShadcnThemeExt::shadcn_theme(ui.ctx());\n\nAreaChart::new(months)\n    .series(AreaSeries {\n        values: vec![186.0, 305.0, 237.0, 73.0, 209.0, 214.0],\n        color: theme.chart_1,\n    })\n    .series(AreaSeries {\n        values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],\n        color: theme.chart_2,\n    })\n    .stacked()\n    .height(260.0)\n    .show(ui);\n\nFlex::row().gap(8.0).show(ui, |f| {\n    f.add(Badge::new(\"Alpha\"));\n    f.add(Badge::new(\"Beta\").variant(BadgeVariant::Success));\n});",
        );
    }
}
