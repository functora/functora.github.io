//! Alert visual variants.

/// Visual variants for the Alert component.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AlertVariant {
    /// Standard informational alert with default colors.
    #[default]
    Default,
    /// Red destructive tint for error messages.
    Destructive,
    /// Green success tint.
    Success,
    /// Amber warning tint.
    Warning,
    /// Blue info tint.
    Info,
}

impl AlertVariant {
    #[must_use]
    pub fn semantic(self) -> Option<crate::tokens::semantic_color::SemanticColor> {
        use crate::tokens::semantic_color::SemanticColor;
        match self {
            Self::Destructive => Some(SemanticColor::Destructive),
            Self::Success => Some(SemanticColor::Success),
            Self::Warning => Some(SemanticColor::Warning),
            Self::Info => Some(SemanticColor::Info),
            Self::Default => None,
        }
    }
}
