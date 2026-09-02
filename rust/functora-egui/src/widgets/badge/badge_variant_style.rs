//! Maps badge variant to concrete style values.

/// Resolves badge colors from variant.
#[must_use]
pub fn resolve_badge_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    variant: crate::tokens::badge_variant::BadgeVariant,
) -> super::resolved_badge_style::ResolvedBadgeStyle {
    match variant {
        crate::tokens::badge_variant::BadgeVariant::Default => {
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: theme.primary,
                fg: theme.primary_foreground,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Secondary => {
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: theme.secondary,
                fg: theme.secondary_foreground,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Destructive => {
            let tint = egui::Color32::from_rgba_unmultiplied(
                theme.destructive.r(),
                theme.destructive.g(),
                theme.destructive.b(),
                26,
            );
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: tint,
                fg: theme.destructive,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Success => {
            let tint = egui::Color32::from_rgba_unmultiplied(
                theme.success.r(),
                theme.success.g(),
                theme.success.b(),
                26,
            );
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: tint,
                fg: theme.success,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Warning => {
            let tint = egui::Color32::from_rgba_unmultiplied(
                theme.warning.r(),
                theme.warning.g(),
                theme.warning.b(),
                26,
            );
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: tint,
                fg: theme.warning,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Info => {
            let tint = egui::Color32::from_rgba_unmultiplied(
                theme.info.r(),
                theme.info.g(),
                theme.info.b(),
                26,
            );
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: tint,
                fg: theme.info,
                border: None,
            }
        }
        crate::tokens::badge_variant::BadgeVariant::Outline => {
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: egui::Color32::TRANSPARENT,
                fg: theme.foreground,
                border: Some(theme.border),
            }
        }
    }
}
