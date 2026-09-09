//! Maps badge variant to concrete style values.

/// Resolves badge colors from variant.
#[must_use]
pub fn resolve_badge_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    variant: crate::tokens::badge_variant::BadgeVariant,
) -> super::resolved_badge_style::ResolvedBadgeStyle {
    if let Some(semantic) = variant.semantic() {
        return super::resolved_badge_style::ResolvedBadgeStyle {
            bg: semantic.tint(theme),
            fg: semantic.color(theme),
            border: None,
        };
    }
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
        crate::tokens::badge_variant::BadgeVariant::Outline => {
            super::resolved_badge_style::ResolvedBadgeStyle {
                bg: egui::Color32::TRANSPARENT,
                fg: theme.foreground,
                border: Some(theme.border),
            }
        }
        _ => super::resolved_badge_style::ResolvedBadgeStyle {
            bg: egui::Color32::TRANSPARENT,
            fg: theme.foreground,
            border: None,
        },
    }
}
