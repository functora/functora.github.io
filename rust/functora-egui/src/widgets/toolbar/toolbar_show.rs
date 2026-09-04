//! Show method for Toolbar.

use crate::responsive::responsive_ext::ResponsiveExt;

impl super::widget::Toolbar {
    /// Renders a compact command bar and calls `content` inside it.
    pub fn show(self, ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let margin = if self.dense {
            egui::Margin {
                left: 6,
                right: 6,
                top: 5,
                bottom: 5,
            }
        } else {
            egui::Margin {
                left: 8,
                right: 8,
                top: 7,
                bottom: 7,
            }
        };

        let frame = egui::Frame::NONE
            .fill(theme.card)
            .inner_margin(margin)
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                theme.radius,
            )))
            .stroke(egui::Stroke::new(1.0, theme.border));

        frame
            .show(ui, |inner_ui| {
                inner_ui.spacing_mut().item_spacing.x = self.spacing;
                inner_ui.spacing_mut().item_spacing.y = self.spacing;
                let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(
                    inner_ui.ctx(),
                );
                let max_h = [
                    crate::tokens::component_size::ComponentSize::Xs,
                    crate::tokens::component_size::ComponentSize::Sm,
                    crate::tokens::component_size::ComponentSize::Default,
                    crate::tokens::component_size::ComponentSize::Lg,
                ]
                .into_iter()
                .map(|s| s.metrics_for(&spacing).0)
                .fold(0.0, f32::max);
                inner_ui.spacing_mut().interact_size.y = max_h;
                let wrap = self.wrap || inner_ui.on_mobile();
                if wrap {
                    let _ = inner_ui.horizontal_wrapped(content);
                } else {
                    let _ = inner_ui.horizontal(content);
                }
            })
            .response
    }
}
