//! Show method for `ButtonGroup` — renders buttons in a connected strip.

impl super::widget::ButtonGroup {
    /// The egui temp data key for the active button group context.
    #[must_use]
    pub fn context_key() -> egui::Id {
        egui::Id::new("functora_egui_btn_group")
    }

    /// Renders a connected button group. Pass buttons inside the closure.
    /// Buttons detect the active context and render with per-corner radii:
    /// the first and the last button of each row get rounded outer corners.
    ///
    /// The strip wraps onto multiple rows when the buttons do not fit the
    /// available width, and each row gets its own ring and dividers.
    pub fn show(ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::InnerResponse<()> {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = theme.radius;
        let key = Self::context_key();

        // Per-group caches (unique per UI position), read from last frame
        let count_key = ui.auto_id_with("btn_group_count");
        let cached_count = ui
            .ctx()
            .data(|d| d.get_temp::<usize>(count_key))
            .unwrap_or(0);
        let rows_key = ui.auto_id_with("btn_group_rows");
        let cached_rows = ui
            .ctx()
            .data(|d| d.get_temp::<Vec<(usize, usize)>>(rows_key))
            .unwrap_or_default();
        let height_key = ui.auto_id_with("btn_group_height");
        let cached_max_height = ui
            .ctx()
            .data(|d| d.get_temp::<f32>(height_key))
            .unwrap_or(0.0);

        // Activate group context
        ui.ctx().data_mut(|d| {
            let _ = d.insert_temp(
                key,
                super::button_group_context::ButtonGroupContext {
                    active: true,
                    rects: Vec::new(),
                    cached_count,
                    current_index: 0,
                    corner_radius: cr,
                    cached_rows,
                    cached_max_height,
                },
            );
        });

        // Wrapping layout for buttons - keep outer spacing for toolbar gap,
        // set inner to 0 for connected buttons. Buttons that do not fit the
        // row move to the next one instead of running off the screen.
        let inline = ui.layout().main_wrap && ui.layout().main_dir.is_horizontal();
        if inline {
            let outer_spacing_x = ui.spacing().item_spacing.x;
            ui.spacing_mut().item_spacing.x = 0.0;
            content(ui);
            ui.spacing_mut().item_spacing.x = outer_spacing_x;
        } else {
            let _ = ui.horizontal_wrapped(|ui_h| {
                ui_h.spacing_mut().item_spacing.x = 0.0;
                content(ui_h);
            });
        }

        // Read the rects, final count, and deactivate
        let (rects, final_count) = ui.ctx().data_mut(|d| {
            let ctx = d.get_temp::<super::button_group_context::ButtonGroupContext>(key);
            let _ = d.insert_temp(
                key,
                super::button_group_context::ButtonGroupContext {
                    active: false,
                    ..Default::default()
                },
            );
            ctx.map(|c| (c.rects, c.current_index)).unwrap_or_default()
        });

        let rows = rows_of(&rects);
        let max_height = rects.iter().map(egui::Rect::height).fold(0.0, f32::max);

        // Cache this group's shape for the next frame
        ui.ctx().data_mut(|d| {
            let _ = d.insert_temp(count_key, final_count);
            let _ = d.insert_temp(rows_key, rows.clone());
            let _ = d.insert_temp(height_key, max_height);
        });

        let group_rect = rects.iter().copied().reduce(egui::Rect::union);
        if inline && let Some(strip) = group_rect {
            let cursor = ui.cursor();
            let row = egui::Rect::from_min_max(
                egui::Pos2::new(strip.min.x, cursor.min.y),
                egui::Pos2::new(strip.max.x, cursor.max.y),
            );
            let _ = ui.advance_cursor_after_rect(row);
        }
        let response = ui.interact(
            group_rect.unwrap_or(egui::Rect::NOTHING),
            ui.auto_id_with("btn_group_ring"),
            egui::Sense::hover(),
        );

        // Ring and dividers, per row of the strip
        let stroke = egui::Stroke::new(1.0, theme.border);
        rows.into_iter()
            .filter_map(|(first, last)| {
                let row_rect = rects
                    .get(first..=last)?
                    .iter()
                    .copied()
                    .reduce(egui::Rect::union)?;
                Some((first, last, row_rect))
            })
            .filter(|(_, _, row_rect)| ui.is_rect_visible(*row_rect))
            .for_each(|(first, last, row_rect)| {
                let _ = ui.painter().rect_stroke(
                    row_rect,
                    crate::paint::corner_radius(cr),
                    stroke,
                    egui::epaint::StrokeKind::Inside,
                );
                if let Some(row) = rects.get(first..=last) {
                    row.windows(2).for_each(|pair| {
                        if let [left, _] = pair {
                            let _ = ui.painter().vline(
                                left.max.x,
                                row_rect.min.y..=row_rect.max.y,
                                stroke,
                            );
                        }
                    });
                }
            });

        egui::InnerResponse {
            inner: (),
            response,
        }
    }
}

/// Splits button rects into rows: a new row starts when the next button
/// wraps back to the left or moves below the previous one.
fn rows_of(rects: &[egui::Rect]) -> Vec<(usize, usize)> {
    let mut rows: Vec<(usize, usize)> = Vec::new();
    for (index, rect) in rects.iter().enumerate() {
        let wraps = index > 0
            && (rect.min.x < rects[index - 1].min.x || rect.min.y > rects[index - 1].min.y);
        match rows.last_mut() {
            Some(row) if !wraps => row.1 = index,
            _ => rows.push((index, index)),
        }
    }
    rows
}
