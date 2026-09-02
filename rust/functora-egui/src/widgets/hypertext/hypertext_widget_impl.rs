//! Widget trait implementation for Hypertext.

use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
use egui::epaint::TextShape;
use egui::{Align, FontId, Stroke, WidgetInfo, WidgetType};

impl egui::Widget for super::widget::Hypertext {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = ShadcnThemeExt::shadcn_theme(ui.ctx());
        let font_id = FontId::proportional(12.0);
        let mut job = egui::text::LayoutJob::default();
        job.wrap.max_width = ui.available_width();
        job.halign = if self.centered {
            Align::Center
        } else {
            Align::LEFT
        };
        job.break_on_newline = true;
        let mut segment_ranges = Vec::new();
        for seg in &self.segments {
            let start = job.text.len();
            match seg {
                super::widget::Segment::Text(t) => {
                    job.append(
                        t,
                        0.0,
                        egui::text::TextFormat {
                            font_id: font_id.clone(),
                            color: theme.muted_foreground,
                            ..Default::default()
                        },
                    );
                }
                super::widget::Segment::Link { label, .. } => {
                    job.append(
                        label,
                        0.0,
                        egui::text::TextFormat {
                            font_id: font_id.clone(),
                            color: theme.primary,
                            ..Default::default()
                        },
                    );
                }
            }
            let end = job.text.len();
            segment_ranges.push(start..end);
        }
        if job.is_empty() {
            return ui.allocate_response(egui::vec2(0.0, 0.0), egui::Sense::hover());
        }
        let galley = ui.fonts_mut(|fonts| fonts.layout_job(job.clone()));
        let available_w = ui.available_width();
        let desired = if self.centered {
            egui::vec2(available_w, galley.size().y)
        } else {
            galley.size()
        };
        let (rect, mut response) = ui.allocate_exact_size(desired, egui::Sense::click());
        response
            .widget_info(|| WidgetInfo::labeled(WidgetType::Label, ui.is_enabled(), galley.text()));
        let galley_pos = match galley.job.halign {
            Align::LEFT => rect.left_top(),
            Align::Center => rect.center_top(),
            Align::RIGHT => rect.right_top(),
        };
        if ui.is_rect_visible(rect) {
            let _ = ui.painter().add(TextShape::new(
                galley_pos,
                std::sync::Arc::clone(&galley),
                egui::Color32::PLACEHOLDER,
            ));
            if let Some(hover_pos) = ui.ctx().input(|i| i.pointer.hover_pos())
                && response.contains_pointer()
                && let Some(hovered) = find_hovered_link(
                    &galley,
                    &job,
                    galley_pos,
                    hover_pos,
                    &segment_ranges,
                    &self.segments,
                )
            {
                ui.set_cursor_icon(egui::CursorIcon::PointingHand);
                paint_underline(
                    ui.painter(),
                    &galley,
                    galley_pos,
                    &job,
                    &segment_ranges,
                    hovered,
                    theme.primary,
                );
            }
        }
        if response.clicked()
            && let Some(hover_pos) = ui.ctx().pointer_interact_pos()
            && let Some(idx) = find_hovered_link(
                &galley,
                &job,
                galley_pos,
                hover_pos,
                &segment_ranges,
                &self.segments,
            )
            && let super::widget::Segment::Link { url, .. } = &self.segments[idx]
        {
            ui.ctx().open_url(egui::OpenUrl::new_tab(url.clone()));
        }
        if response.hovered()
            && find_hovered_link(
                &galley,
                &job,
                galley_pos,
                ui.ctx()
                    .input(|i| i.pointer.hover_pos().unwrap_or(egui::pos2(0.0, 0.0))),
                &segment_ranges,
                &self.segments,
            )
            .is_some()
        {
            response = response.on_hover_cursor(egui::CursorIcon::PointingHand);
        }
        response
    }
}

fn find_hovered_link(
    galley: &egui::Galley,
    job: &egui::text::LayoutJob,
    galley_pos: egui::Pos2,
    hover_pos: egui::Pos2,
    segment_ranges: &[std::ops::Range<usize>],
    segments: &[super::widget::Segment],
) -> Option<usize> {
    if !galley
        .rect
        .translate(galley_pos.to_vec2())
        .contains(hover_pos)
    {
        return None;
    }
    let rel = hover_pos - galley_pos;
    let cursor = galley.cursor_from_pos(egui::vec2(rel.x, rel.y));
    let char_idx = cursor.index.0;
    let byte_offset = job
        .text
        .char_indices()
        .nth(char_idx)
        .map_or(job.text.len(), |(b, _)| b);
    let mut seg_idx = None;
    for (idx, range) in segment_ranges.iter().enumerate() {
        if range.contains(&byte_offset) {
            seg_idx = Some(idx);
            break;
        }
        if byte_offset == job.text.len()
            && idx + 1 == segment_ranges.len()
            && range.contains(&byte_offset.saturating_sub(1))
        {
            seg_idx = Some(idx);
            break;
        }
    }
    let idx = seg_idx?;
    match &segments[idx] {
        super::widget::Segment::Link { .. } => Some(idx),
        super::widget::Segment::Text(_) => None,
    }
}

fn paint_underline(
    painter: &egui::Painter,
    galley: &egui::Galley,
    galley_pos: egui::Pos2,
    job: &egui::text::LayoutJob,
    segment_ranges: &[std::ops::Range<usize>],
    hovered_idx: usize,
    color: egui::Color32,
) {
    let Some(range) = segment_ranges.get(hovered_idx) else {
        return;
    };
    let char_to_byte: Vec<usize> = job.text.char_indices().map(|(b, _)| b).collect();
    let mut global_char: usize = 0;
    for row in &galley.rows {
        let mut min_x: Option<f32> = None;
        let mut max_x: Option<f32> = None;
        for glyph in &row.glyphs {
            let byte_offset = char_to_byte
                .get(global_char)
                .copied()
                .unwrap_or(job.text.len());
            if range.contains(&byte_offset) {
                let left = glyph.pos.x + row.pos.x;
                let right = glyph.pos.x + glyph.advance_width + row.pos.x;
                min_x = Some(min_x.map_or(left, |v| v.min(left)));
                max_x = Some(max_x.map_or(right, |v| v.max(right)));
            }
            global_char += 1;
        }
        if row.ends_with_newline {
            let newline_byte = char_to_byte
                .get(global_char)
                .copied()
                .unwrap_or(job.text.len());
            if range.contains(&newline_byte) {
                global_char += 1;
            } else if !row.glyphs.is_empty() {
                let last_byte = char_to_byte
                    .get(global_char.saturating_sub(1))
                    .copied()
                    .unwrap_or(0);
                if range.contains(&last_byte) {
                    // newline after link, ignore
                }
                global_char += 1;
            } else {
                global_char += 1;
            }
        }
        if let (Some(l), Some(r)) = (min_x, max_x) {
            let y = galley_pos.y + row.pos.y + row.size.y - 2.0;
            let _ = painter.hline(
                (galley_pos.x + l)..=(galley_pos.x + r),
                y,
                Stroke::new(1.0, color),
            );
        }
    }
}
