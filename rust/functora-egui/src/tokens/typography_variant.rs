//! Typography level variants.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TypographyVariant {
    H1,
    H2,
    H3,
    H4,
    #[default]
    P,
    Lead,
    Large,
    Small,
    Muted,
}

impl TypographyVariant {
    /// Font size of this typography level, in points.
    #[must_use]
    pub const fn font_size(self) -> f32 {
        match self {
            Self::H1 => 36.0,
            Self::H2 => 30.0,
            Self::H3 => 24.0,
            Self::H4 | Self::Lead => 20.0,
            Self::P | Self::Muted => 14.0,
            Self::Large => 18.0,
            Self::Small => 13.0,
        }
    }

    /// Foreground color of this typography level.
    #[must_use]
    pub fn foreground(self, theme: &crate::theme::shadcn_theme::ShadcnTheme) -> egui::Color32 {
        match self {
            Self::Muted | Self::Lead => theme.muted_foreground,
            Self::H1 | Self::H2 | Self::H3 | Self::H4 | Self::P | Self::Large | Self::Small => {
                theme.foreground
            }
        }
    }
}
