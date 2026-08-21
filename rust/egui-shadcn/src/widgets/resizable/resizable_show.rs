//! Show method for Resizable -- renders a draggable split panel.

use crate::responsive::responsive_ext::ResponsiveExt;

impl super::resizable::Resizable {
    /// Shows a horizontal split with draggable divider.
    /// `fraction` persists the split position. Pass `&mut your_f32_state`.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        fraction: &mut f32,
        left: impl FnOnce(&mut egui::Ui),
        right: impl FnOnce(&mut egui::Ui),
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let available_width = ui.available_width();
        let touch = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx())
            .touch_height;
        let handle_width: f32 = if ui.on_mobile() { touch.max(24.0) } else { 8.0 };
        let panel_height = self.height;

        let left_width = ((available_width - handle_width) * (*fraction)).max(0.0);
        let right_width = (available_width - left_width - handle_width).max(0.0);

        let origin = ui.cursor().min;
        let left_rect = egui::Rect::from_min_size(origin, egui::vec2(left_width, panel_height));
        let handle_rect = egui::Rect::from_min_size(
            egui::pos2(left_rect.right(), origin.y),
            egui::vec2(handle_width, panel_height),
        );
        let right_rect = egui::Rect::from_min_size(
            egui::pos2(handle_rect.right(), origin.y),
            egui::vec2(right_width, panel_height),
        );

        let (whole_rect, whole_response) = ui.allocate_exact_size(
            egui::vec2(available_width, panel_height),
            egui::Sense::hover(),
        );

        if ui.is_rect_visible(whole_rect) {
            let painter = ui.painter();
            painter.rect_filled(handle_rect, egui::CornerRadius::ZERO, theme.border);

            let center = handle_rect.center();
            let dot_color = theme.muted_foreground;
            for dy in [-8.0_f32, 0.0, 8.0] {
                painter.circle_filled(egui::pos2(center.x, center.y + dy), 1.5, dot_color);
            }
        }

        let handle_id = ui.id().with("resizable_handle");
        let handle_response = ui.interact(handle_rect, handle_id, egui::Sense::drag());

        if handle_response.dragged() {
            let delta = handle_response.drag_delta().x;
            let total = available_width - handle_width;
            *fraction = (*fraction + delta / total).clamp(0.1, 0.9);
            ui.ctx().request_repaint();
        }

        if handle_response.hovered() || handle_response.dragged() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeColumn);
        }

        let mut left_ui = ui.new_child(
            egui::UiBuilder::new()
                .max_rect(left_rect)
                .id_salt("resizable_left"),
        );
        left(&mut left_ui);

        let mut right_ui = ui.new_child(
            egui::UiBuilder::new()
                .max_rect(right_rect)
                .id_salt("resizable_right"),
        );
        right(&mut right_ui);

        whole_response
    }
}
