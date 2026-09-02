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
