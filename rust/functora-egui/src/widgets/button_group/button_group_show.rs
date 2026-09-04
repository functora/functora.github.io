//! Show method for `ButtonGroup` — renders buttons in a connected horizontal strip.

impl super::widget::ButtonGroup {
    /// The egui temp data key for the active button group context.
    #[must_use]
    pub fn context_key() -> egui::Id {
        egui::Id::new("functora_egui_btn_group")
    }

    /// Renders a connected button group. Pass buttons inside the closure.
    /// Buttons detect the active context and render with per-corner radii:
    /// first button gets left rounding, last button gets right rounding.
    pub fn show(ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::InnerResponse<()> {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = theme.radius;
        let key = Self::context_key();

        // Per-group count cache (unique per UI position)
        let count_key = ui.auto_id_with("btn_group_count");
        let cached_count = ui
            .ctx()
            .data(|d| d.get_temp::<usize>(count_key))
            .unwrap_or(0);

        // Activate group context
        ui.ctx().data_mut(|d| {
            let _ = d.insert_temp(
                key,
                super::button_group_context::ButtonGroupContext {
                    active: true,
                    boundaries: Vec::new(),
                    cached_count,
                    current_index: 0,
                    corner_radius: cr,
                    group_rect: None,
                },
            );
        });

        // Use horizontal layout for buttons - keep outer spacing for toolbar gap, set inner to 0 for connected buttons
        let _ = ui.horizontal(|ui_h| {
            ui_h.spacing_mut().item_spacing.x = 0.0;
            content(ui_h);
        });

        // Read boundaries, group rect, final count, and deactivate
        let (boundaries, group_rect, final_count) = ui.ctx().data_mut(|d| {
            let ctx = d.get_temp::<super::button_group_context::ButtonGroupContext>(key);
            let _ = d.insert_temp(
                key,
                super::button_group_context::ButtonGroupContext {
                    active: false,
                    ..Default::default()
                },
            );
            ctx.map(|c| (c.boundaries, c.group_rect, c.current_index))
                .unwrap_or_default()
        });

        // Cache this group's count for next frame
        let _ = ui.ctx().data_mut(|d| d.insert_temp(count_key, final_count));

        // Draw outer ring and dividers using the union rect from buttons
        let rect = group_rect.unwrap_or(egui::Rect::NOTHING);
        let response = ui.interact(
            rect,
            ui.auto_id_with("btn_group_ring"),
            egui::Sense::hover(),
        );

        if ui.is_rect_visible(rect) {
            // Outer rounded border
            let _ = ui.painter().rect_stroke(
                rect,
                egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(cr)),
                egui::Stroke::new(1.0, theme.border),
                egui::epaint::StrokeKind::Inside,
            );

            // Vertical dividers between buttons (skip after the last one)
            if boundaries.len() > 1 {
                for &x in boundaries.iter().take(boundaries.len() - 1) {
                    let _ = ui.painter().vline(
                        x,
                        rect.min.y..=rect.max.y,
                        egui::Stroke::new(1.0, theme.border),
                    );
                }
            }
        }

        egui::InnerResponse {
            inner: (),
            response,
        }
    }
}
