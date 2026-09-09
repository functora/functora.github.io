//! Semantic color roles shared by badge, alert, toast and other variants.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticColor {
    Destructive,
    Success,
    Warning,
    Info,
}

impl SemanticColor {
    #[must_use]
    pub fn color(self, theme: &crate::theme::shadcn_theme::ShadcnTheme) -> egui::Color32 {
        match self {
            Self::Destructive => theme.destructive,
            Self::Success => theme.success,
            Self::Warning => theme.warning,
            Self::Info => theme.info,
        }
    }

    /// 10% alpha tint for soft backgrounds (shadcn convention).
    #[must_use]
    pub fn tint(self, theme: &crate::theme::shadcn_theme::ShadcnTheme) -> egui::Color32 {
        crate::utils::with_alpha(self.color(theme), 26)
    }
}
