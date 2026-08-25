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
