//! Widget trait implementation for Typography.

impl egui::Widget for super::widget::Typography {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let (font_size, _line_height, is_bold) = self.variant.metrics();

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

        let mut rich_text = egui::RichText::new(self.text).color(color).size(font_size);
        if is_bold {
            rich_text = rich_text.strong();
        }

        ui.label(rich_text)
    }
}
