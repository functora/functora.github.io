use functora_egui::snippet;
use functora_egui::{AspectRatio, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_aspect_ratio(ui: &mut egui::Ui) {
        _ = Typography::muted("Maintains a fixed width-to-height ratio.").show(ui);
        ui.add_space(12.0);
        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
        ui.set_max_width(ui.available_width());
        _ = AspectRatio::new(16.0 / 9.0).show(ui, |ui50| {
            let rect = ui50.available_rect_before_wrap();
            _ = ui50.painter().rect_filled(
                rect,
                egui::CornerRadius::from(theme.radius),
                theme.muted,
            );
            let galley = ui50.painter().layout_no_wrap(
                "16:9".to_owned(),
                egui::FontId::proportional(20.0),
                theme.muted_foreground,
            );
            ui50.painter().galley(
                egui::pos2(
                    rect.center().x - galley.size().x / 2.0,
                    rect.center().y - galley.size().y / 2.0,
                ),
                galley,
                theme.muted_foreground,
            );
        });
        ui.add_space(8.0);
        _ = AspectRatio::new(1.0).show(ui, |ui51| {
            let rect = ui51.available_rect_before_wrap();
            _ = ui51.painter().rect_filled(
                rect,
                egui::CornerRadius::from(theme.radius),
                theme.muted,
            );
            let galley = ui51.painter().layout_no_wrap(
                "1:1".to_owned(),
                egui::FontId::proportional(20.0),
                theme.muted_foreground,
            );
            ui51.painter().galley(
                egui::pos2(
                    rect.center().x - galley.size().x / 2.0,
                    rect.center().y - galley.size().y / 2.0,
                ),
                galley,
                theme.muted_foreground,
            );
        });

        snippet(
            ui,
            "// AspectRatio: maintains fixed width/height ratio\nuse functora_egui::AspectRatio;\n\n// 16:9 video player\nAspectRatio::new(16.0 / 9.0).show(ui, |ui| {\n    // ui.available_rect_before_wrap() is 16:9\n    ui.label(\"16:9\");\n});\n\n// 1:1 square\nAspectRatio::new(1.0).show(ui, |ui| {\n    ui.label(\"1:1\");\n});",
        );
    }
}
