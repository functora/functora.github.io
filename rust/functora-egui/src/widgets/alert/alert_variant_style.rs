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
        crate::tokens::alert_variant::AlertVariant::Destructive => {
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: theme.destructive,
                border: theme.destructive,
            }
        }
        crate::tokens::alert_variant::AlertVariant::Success => {
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: theme.success,
                border: theme.success,
            }
        }
        crate::tokens::alert_variant::AlertVariant::Warning => {
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: theme.warning,
                border: theme.warning,
            }
        }
        crate::tokens::alert_variant::AlertVariant::Info => {
            super::resolved_alert_style::ResolvedAlertStyle {
                bg: theme.background,
                fg: theme.info,
                border: theme.info,
            }
        }
    }
}
