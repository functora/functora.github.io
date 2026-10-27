use crate::icons::lucide_icon::LucideIcon;
use crate::widgets::paste_clear_core::{
    apply_clear, copy_pending, maybe_pending_color, paint_focused, paint_outer, paint_tool_icon,
    paste_pending, poll_copy, poll_paste, request_copy, request_paste, respond, take_slots,
    tool_color,
};
use crate::widgets::textarea::textarea_widget_impl::wrap_anywhere_layouter_with_color;

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
        readonly,
        max_height,
    } = widget;
    let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
    let h_padding: f32 = spacing.touch_padding;
    let v_padding: f32 = 8.0;
    let width = ui.available_width();

    let toolbar_h: f32 = 28.0;
    let text_color = theme.foreground;
    let text_height =
        wrap_anywhere_layouter_with_color(ui, text, (width - h_padding * 2.0).max(0.0), text_color)
            .size()
            .y;
    let fitted = toolbar_h + v_padding + text_height + v_padding;
    let outer_height = fitted.max(min_height).min(max_height);
    let desired = egui::vec2(width, outer_height);
    let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::click());
    let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

    paint_outer(ui, outer_rect, &theme, outer_hovered, readonly);

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

    let paste_rect = if readonly {
        None
    } else {
        Some(egui::Rect::from_min_max(
            egui::pos2(toolbar_rect.min.x + 4.0, toolbar_rect.min.y + 4.0),
            egui::pos2(toolbar_rect.min.x + 32.0, toolbar_rect.max.y - 4.0),
        ))
    };
    let copy_left = if readonly { 4.0 } else { 36.0 };
    let copy_rect = if copy {
        Some(egui::Rect::from_min_max(
            egui::pos2(toolbar_rect.min.x + copy_left, toolbar_rect.min.y + 4.0),
            egui::pos2(
                toolbar_rect.min.x + copy_left + 28.0,
                toolbar_rect.max.y - 4.0,
            ),
        ))
    } else {
        None
    };
    let clear_rect = if readonly {
        None
    } else {
        Some(egui::Rect::from_min_max(
            egui::pos2(toolbar_rect.max.x - 32.0, toolbar_rect.min.y + 4.0),
            egui::pos2(toolbar_rect.max.x - 4.0, toolbar_rect.max.y - 4.0),
        ))
    };

    let copy_resp = copy_rect.map(|r| ui.interact(r, copy_id, egui::Sense::click()));
    let paste_resp = paste_rect.map(|r| ui.interact(r, paste_id, egui::Sense::click()));
    let clear_resp = clear_rect.map(|r| ui.interact(r, clear_id, egui::Sense::click()));

    crate::widgets::paste_clear_core::tool_tooltip(
        paste_resp.as_ref(),
        &crate::messages::Msg::Paste,
        ui.ctx(),
    );
    crate::widgets::paste_clear_core::tool_tooltip(
        copy_resp.as_ref(),
        &crate::messages::Msg::Copy,
        ui.ctx(),
    );
    crate::widgets::paste_clear_core::tool_tooltip(
        clear_resp.as_ref(),
        &crate::messages::Msg::Clear,
        ui.ctx(),
    );

    let is_paste_pending = paste_pending(&slots.paste);
    let is_copy_pending = copy_pending(&slots.copy);

    if let Some(resp) = paste_resp.as_ref()
        && resp.clicked()
    {
        request_paste(ui, &slots.paste);
    }

    if let Some(resp) = &copy_resp
        && resp.clicked()
    {
        request_copy(ui, &slots.copy, text);
    }

    let mut cleared = false;
    if let Some(resp) = clear_resp.as_ref()
        && resp.clicked()
    {
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
        let color = maybe_pending_color(
            tool_color(&theme, resp.hovered(), !text.is_empty()),
            is_copy_pending,
        );
        paint_tool_icon(ui.painter(), rect, copy_icon, color);
    }

    if let Some(rect) = paste_rect
        && let Some(resp) = &paste_resp
        && ui.is_rect_visible(rect)
    {
        let color = maybe_pending_color(
            tool_color(&theme, resp.hovered(), !readonly),
            is_paste_pending,
        );
        paint_tool_icon(ui.painter(), rect, paste_icon, color);
    }

    if let Some(rect) = clear_rect
        && let Some(resp) = &clear_resp
        && ui.is_rect_visible(rect)
    {
        let clear_enabled = *text != default_value;
        let icon = if clear_enabled {
            clear_icon
        } else {
            LucideIcon::X
        };
        let color = tool_color(&theme, resp.hovered(), clear_enabled);
        paint_tool_icon(ui.painter(), rect, icon, color);
    }

    let mut child_ui = ui.new_child(
        egui::UiBuilder::new()
            .max_rect(content_rect)
            .layout(egui::Layout::top_down(egui::Align::LEFT)),
    );

    let scroll_resp = egui::ScrollArea::vertical()
        .max_height(content_rect.height())
        .show(&mut child_ui, |inner_ui| {
            let mut wrap =
                move |inner: &egui::Ui, buffer: &dyn egui::TextBuffer, wrap_width: f32| {
                    wrap_anywhere_layouter_with_color(inner, buffer, wrap_width, text_color)
                };
            let text_edit = egui::TextEdit::multiline(text)
                .frame(egui::Frame::NONE)
                .hint_text(&placeholder)
                .text_color(text_color)
                .interactive(!readonly)
                .desired_width(content_rect.width())
                .desired_rows(1)
                .min_size(content_rect.size())
                .layouter(&mut wrap);
            inner_ui.add(text_edit)
        });

    let outer_clicked = outer_response.clicked();
    let response = if readonly {
        scroll_resp.inner
    } else {
        let mut united = scroll_resp.inner.union(outer_response);
        united.rect = outer_rect;
        united.interact_rect = outer_rect;
        united
    };

    if !readonly && outer_clicked && !response.has_focus() {
        response.request_focus();
    }

    if readonly && copy {
        let field_copy_id = base_id.with("field_copy");
        if ui
            .interact(content_rect, field_copy_id, egui::Sense::click())
            .clicked()
        {
            request_copy(ui, &slots.copy, text);
        }
    }

    if response.has_focus() {
        paint_focused(ui, outer_rect, &theme);
    }

    respond(response, pasted, copied, cleared, paste_error, copy_error)
}
