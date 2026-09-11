use crate::icons::lucide_icon::LucideIcon;
use crate::widgets::paste_clear_core::{
    apply_clear, copy_pending, disabled_color, hover_color, paint_focused, paint_outer,
    paint_tool_icon, paste_pending, pending_color, poll_copy, poll_paste, request_copy,
    request_paste, respond, take_slots,
};

pub use crate::widgets::paste_clear_core::PasteClearResponse;

pub(crate) fn show_textarea_paste_clear(
    ui: &mut egui::Ui,
    widget: super::widget::TextareaPasteClear<'_>,
) -> PasteClearResponse {
    let super::widget::TextareaPasteClear {
        text,
        placeholder,
        default_value,
        min_height,
        paste_icon,
        clear_icon,
        copy,
        copy_icon,
    } = widget;
    let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
    let h_padding: f32 = spacing.touch_padding;
    let v_padding: f32 = 8.0;
    let width = ui.available_width();

    let desired = egui::vec2(width, min_height);
    let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::hover());
    let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

    paint_outer(ui, outer_rect, &theme, outer_hovered);

    let toolbar_h: f32 = 28.0;
    let toolbar_rect = egui::Rect::from_min_max(
        outer_rect.min,
        egui::pos2(outer_rect.max.x, outer_rect.min.y + toolbar_h),
    );
    let content_rect = egui::Rect::from_min_max(
        egui::pos2(
            outer_rect.min.x + h_padding,
            outer_rect.min.y + toolbar_h + v_padding,
        ),
        egui::pos2(outer_rect.max.x - h_padding, outer_rect.max.y - v_padding),
    );

    let base_id = ui.auto_id_with("textarea_paste_clear");
    let paste_id = base_id.with("paste_btn");
    let clear_id = base_id.with("clear_btn");
    let copy_id = base_id.with("copy_btn");

    let slots = take_slots(ui, base_id);

    let (pasted, paste_error) = poll_paste(ui, &slots.paste, &mut *text);
    let (copied, copy_error) = poll_copy(ui, &slots.copy);

    let paste_rect = egui::Rect::from_min_max(
        egui::pos2(toolbar_rect.min.x + 4.0, toolbar_rect.min.y + 4.0),
        egui::pos2(toolbar_rect.min.x + 32.0, toolbar_rect.max.y - 4.0),
    );
    let copy_rect = if copy {
        Some(egui::Rect::from_min_max(
            egui::pos2(toolbar_rect.min.x + 36.0, toolbar_rect.min.y + 4.0),
            egui::pos2(toolbar_rect.min.x + 64.0, toolbar_rect.max.y - 4.0),
        ))
    } else {
        None
    };
    let clear_rect = egui::Rect::from_min_max(
        egui::pos2(toolbar_rect.max.x - 32.0, toolbar_rect.min.y + 4.0),
        egui::pos2(toolbar_rect.max.x - 4.0, toolbar_rect.max.y - 4.0),
    );

    let copy_resp = copy_rect.map(|r| ui.interact(r, copy_id, egui::Sense::click()));
    let paste_resp = ui.interact(paste_rect, paste_id, egui::Sense::click());
    let clear_resp = ui.interact(clear_rect, clear_id, egui::Sense::click());

    let is_paste_pending = paste_pending(&slots.paste);
    let is_copy_pending = copy_pending(&slots.copy);

    if paste_resp.clicked() {
        request_paste(ui, &slots.paste);
    }

    if let Some(resp) = &copy_resp
        && resp.clicked()
    {
        request_copy(ui, &slots.copy, text);
    }

    let mut cleared = false;
    if clear_resp.clicked() {
        cleared = apply_clear(text, &default_value);
    }

    let _ = ui.painter().hline(
        toolbar_rect.min.x + h_padding..=toolbar_rect.max.x - h_padding,
        toolbar_rect.max.y,
        egui::Stroke::new(1.0, theme.border),
    );

    if let Some(rect) = copy_rect
        && let Some(resp) = &copy_resp
        && ui.is_rect_visible(rect)
    {
        let base_color = if text.is_empty() {
            disabled_color(&theme)
        } else {
            hover_color(&theme, resp.hovered())
        };
        let display_color = if is_copy_pending {
            pending_color(base_color)
        } else {
            base_color
        };
        paint_tool_icon(ui.painter(), rect, copy_icon, display_color);
    }

    if ui.is_rect_visible(paste_rect) {
        let base_paste_color = hover_color(&theme, paste_resp.hovered());
        let display_paste_color = if is_paste_pending {
            pending_color(base_paste_color)
        } else {
            base_paste_color
        };
        paint_tool_icon(ui.painter(), paste_rect, paste_icon, display_paste_color);
    }

    if ui.is_rect_visible(clear_rect) {
        let clear_enabled = *text != default_value;
        let clear_color = if clear_enabled {
            hover_color(&theme, clear_resp.hovered())
        } else {
            disabled_color(&theme)
        };
        let icon = if clear_enabled {
            clear_icon
        } else {
            LucideIcon::X
        };
        paint_tool_icon(ui.painter(), clear_rect, icon, clear_color);
    }

    let mut child_ui = ui.new_child(
        egui::UiBuilder::new()
            .max_rect(content_rect)
            .layout(egui::Layout::top_down(egui::Align::LEFT)),
    );

    let scroll_resp = egui::ScrollArea::vertical()
        .max_height(content_rect.height())
        .show(&mut child_ui, |inner_ui| {
            let text_edit = egui::TextEdit::multiline(text)
                .frame(egui::Frame::NONE)
                .hint_text(&placeholder)
                .text_color(theme.foreground)
                .desired_width(content_rect.width())
                .desired_rows(8);
            inner_ui.add(text_edit)
        });

    let response = scroll_resp.inner;

    if outer_response.clicked() && !response.has_focus() {
        response.request_focus();
    }

    if response.has_focus() {
        paint_focused(ui, outer_rect, &theme);
    }

    respond(response, pasted, copied, cleared, paste_error, copy_error)
}
