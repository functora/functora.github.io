use functora_egui::snippet;
use functora_egui::{Carousel, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_carousel(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A slider with prev/next navigation and dots.").show(ui);
        ui.add_space(12.0);
        let slides = [
            ("Slide 1", egui::Color32::from_rgb(25, 113, 194)),
            ("Slide 2", egui::Color32::from_rgb(18, 184, 134)),
            ("Slide 3", egui::Color32::from_rgb(245, 159, 0)),
            ("Slide 4", egui::Color32::from_rgb(224, 49, 49)),
        ];
        let total = slides.len();
        _ = Carousel::new(total).show(ui, &mut self.carousel_idx, |slide_ui, idx| {
            if let Some((name, color)) = slides.get(idx).copied() {
                let width = slide_ui.available_width().min(420.0);
                let (rect, _) =
                    slide_ui.allocate_exact_size(egui::vec2(width, 200.0), egui::Sense::hover());
                if slide_ui.is_rect_visible(rect) {
                    let theme = functora_egui::ShadcnThemeExt::shadcn_theme(slide_ui.ctx());
                    let painter = slide_ui.painter();
                    _ = painter.rect_filled(rect, egui::CornerRadius::from(theme.radius), color);
                    let galley = painter.layout_no_wrap(
                        name.to_owned(),
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
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!("Slide {} of {total}", self.carousel_idx + 1)).show(ui);

        snippet(
            ui,
            "// Carousel: slider with prev/next + dots\nuse functora_egui::Carousel;\n\nlet slides = [(\"Slide 1\", Color32::from_rgb(25, 113, 194)), (\"Slide 2\", Color32::from_rgb(18, 184, 134)), (\"Slide 3\", Color32::from_rgb(245, 159, 0)), (\"Slide 4\", Color32::from_rgb(224, 49, 49))];\nlet mut index = 0;\n\nCarousel::new(slides.len()).show(ui, &mut index, |slide, idx| {\n    if let Some((name, color)) = slides.get(idx).copied() {\n        let width = slide.available_width().min(420.0);\n        let (rect, _) = slide.allocate_exact_size(egui::vec2(width, 200.0), egui::Sense::hover());\n        if slide.is_rect_visible(rect) {\n            let theme = ShadcnThemeExt::shadcn_theme(slide.ctx());\n            let painter = slide.painter();\n            let _ = painter.rect_filled(rect, egui::CornerRadius::from(theme.radius), color);\n            let galley = painter.layout_no_wrap(name.to_owned(), egui::FontId::proportional(24.0), egui::Color32::WHITE);\n            painter.galley(\n                egui::pos2(rect.center().x - galley.size().x / 2.0, rect.center().y - galley.size().y / 2.0),\n                galley,\n                egui::Color32::WHITE,\n            );\n        }\n    }\n});",
        );
    }
}
