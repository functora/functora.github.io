use functora_egui::snippet;
use functora_egui::{Pagination, ResponsiveExt, Typography};

impl crate::state::ShowcaseApp {
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
}
