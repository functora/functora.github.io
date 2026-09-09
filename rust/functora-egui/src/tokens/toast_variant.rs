//! Toast visual variants.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ToastVariant {
    #[default]
    Default,
    Success,
    Error,
    Warning,
    Info,
}

impl ToastVariant {
    #[must_use]
    pub fn semantic(self) -> Option<crate::tokens::semantic_color::SemanticColor> {
        use crate::tokens::semantic_color::SemanticColor;
        match self {
            Self::Error => Some(SemanticColor::Destructive),
            Self::Success => Some(SemanticColor::Success),
            Self::Warning => Some(SemanticColor::Warning),
            Self::Info => Some(SemanticColor::Info),
            Self::Default => None,
        }
    }
}
