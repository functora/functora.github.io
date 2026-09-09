//! Badge style variants.

/// Visual variants for the Badge component.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BadgeVariant {
    /// Solid primary background.
    #[default]
    Default,
    /// Muted secondary background.
    Secondary,
    /// Red destructive tint.
    Destructive,
    /// Green success tint.
    Success,
    /// Amber warning tint.
    Warning,
    /// Blue info tint.
    Info,
    /// Border only, transparent background.
    Outline,
}

impl BadgeVariant {
    #[must_use]
    pub fn semantic(self) -> Option<crate::tokens::semantic_color::SemanticColor> {
        use crate::tokens::semantic_color::SemanticColor;
        match self {
            Self::Destructive => Some(SemanticColor::Destructive),
            Self::Success => Some(SemanticColor::Success),
            Self::Warning => Some(SemanticColor::Warning),
            Self::Info => Some(SemanticColor::Info),
            Self::Default | Self::Secondary | Self::Outline => None,
        }
    }
}
