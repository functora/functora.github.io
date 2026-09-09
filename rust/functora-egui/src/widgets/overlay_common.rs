//! Shared building blocks for modal overlays (dialog, alert dialog, sheet,
//! blocking overlay): a dimmed click-to-dismiss backdrop and a close button.

pub(crate) const BACKDROP_ALPHA: u8 = 60;

pub(crate) const OVERLAY_CLOSE_SIZE: f32 = 16.0;

/// Paints a dimmed backdrop over the whole viewport and returns the response
/// of the full-screen click area behind the panel.
pub(crate) fn paint_backdrop(ctx: &egui::Context, id_suffix: &str, alpha: u8) -> egui::Response {
    let screen = ctx.input(egui::InputState::viewport_rect);
    let layer = egui::LayerId::new(egui::Order::Middle, egui::Id::new(id_suffix));
    let _ = ctx.layer_painter(layer).rect_filled(
        screen,
        egui::CornerRadius::ZERO,
        egui::Color32::from_black_alpha(alpha),
    );
    egui::Area::new(egui::Id::new(id_suffix).with("sense"))
        .order(egui::Order::Middle)
        .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
        .show(ctx, |inner_ui| {
            inner_ui
                .allocate_exact_size(screen.size(), egui::Sense::click())
                .1
        })
        .inner
}

/// Renders the standard top-right X close button for overlays; sets
/// `*open = false` and requests a repaint when clicked.
pub(crate) fn close_button(
    ui: &mut egui::Ui,
    ctx: &egui::Context,
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    open: &mut bool,
) {
    let _ = ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |inner_ui| {
        let (rect, resp) = inner_ui.allocate_exact_size(
            egui::vec2(OVERLAY_CLOSE_SIZE, OVERLAY_CLOSE_SIZE),
            egui::Sense::click(),
        );
        if inner_ui.is_rect_visible(rect) {
            crate::icons::paint_icon::paint_icon(
                inner_ui.painter(),
                rect,
                &crate::icons::lucide_icon::LucideIcon::X,
                theme.muted_foreground,
            );
        }
        let close_clicked = resp
            .on_hover_cursor(egui::CursorIcon::PointingHand)
            .clicked();
        if close_clicked {
            *open = false;
            ctx.request_repaint();
        }
    });
}
