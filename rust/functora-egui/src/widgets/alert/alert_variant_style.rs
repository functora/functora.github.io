//! Maps `AlertVariant` to concrete style values.

/// Resolves alert colors from variant.
#[must_use]
pub fn resolve_alert_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    variant: crate::tokens::alert_variant::AlertVariant,
) -> super::resolved_alert_style::ResolvedAlertStyle {
    match variant {
        crate::tokens::alert_variant::AlertVariant::Default => {
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: theme.foreground,
                border: theme.border,
            }
        }
        crate::tokens::alert_variant::AlertVariant::Destructive
        | crate::tokens::alert_variant::AlertVariant::Success
        | crate::tokens::alert_variant::AlertVariant::Warning
        | crate::tokens::alert_variant::AlertVariant::Info => {
            let color = variant
                .semantic()
                .map_or(theme.foreground, |s| s.color(theme));
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: color,
                border: color,
            }
        }
    }
}
