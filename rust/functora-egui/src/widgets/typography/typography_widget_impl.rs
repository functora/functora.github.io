//! Widget trait implementation for Typography.

impl egui::Widget for super::widget::Typography {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let (font_size, _line_height, _is_bold) = self.variant.metrics();

        let color = match self.variant {
            crate::tokens::typography_variant::TypographyVariant::Muted
            | crate::tokens::typography_variant::TypographyVariant::Lead => theme.muted_foreground,
            crate::tokens::typography_variant::TypographyVariant::H1
            | crate::tokens::typography_variant::TypographyVariant::H2
            | crate::tokens::typography_variant::TypographyVariant::H3
            | crate::tokens::typography_variant::TypographyVariant::H4
            | crate::tokens::typography_variant::TypographyVariant::P
            | crate::tokens::typography_variant::TypographyVariant::Large
            | crate::tokens::typography_variant::TypographyVariant::Small => theme.foreground,
        };

        let galley = crate::text::layout(
            ui,
            self.text,
            egui::FontId::proportional(font_size),
            color,
            ui.available_width(),
            None,
        );
        let mut response = ui.label(galley.clone());
        let intrinsic = galley.intrinsic_size();
        response.set_intrinsic_size(egui::vec2(
            intrinsic.x,
            intrinsic.y.max(response.rect.height()),
        ));
        response
    }
}
