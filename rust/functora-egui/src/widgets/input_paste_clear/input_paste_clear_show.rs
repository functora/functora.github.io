use crate::icons::lucide_icon::LucideIcon;
use crate::widgets::paste_clear_core::{
    apply_clear, copy_pending, disabled_color, hover_color, paint_focused, paint_outer,
    paint_tool_icon, paste_pending, pending_color, poll_copy, poll_paste, request_copy,
    request_paste, respond, take_slots,
};

pub use crate::widgets::paste_clear_core::PasteClearResponse;

pub(crate) fn show_input_paste_clear(
    ui: &mut egui::Ui,
    widget: super::widget::InputPasteClear<'_>,
) -> PasteClearResponse {
    let super::widget::InputPasteClear {
        text,
        placeholder,
        default_value,
        password,
        paste_icon,
        clear_icon,
        copy,
        copy_icon,
    } = widget;
    let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
    let height = spacing.touch_height;
    let h_padding = spacing.touch_padding;

    let width = ui.available_width();
    let desired = egui::vec2(width, height);
    let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::hover());
    let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

    paint_outer(ui, outer_rect, &theme, outer_hovered);

    let copy_width: f32 = if copy { 40.0 } else { 0.0 };
    let paste_width: f32 = 40.0;
    let clear_width: f32 = 40.0;
    let eye_width: f32 = 32.0;
    let has_eye = password;
    let right_reserve = if has_eye {
        clear_width + eye_width
    } else {
        clear_width
    };
    let left_reserve = copy_width + paste_width;

    let base_id = ui.auto_id_with("input_paste_clear");
    let paste_id = base_id.with("paste_btn");
    let clear_id = base_id.with("clear_btn");
    let reveal_id = base_id.with("reveal");
    let eye_id = base_id.with("eye_btn");
    let copy_id = base_id.with("copy_btn");

    let slots = take_slots(ui, base_id);

    let (pasted, paste_error) = poll_paste(ui, &slots.paste, &mut *text);
    let (copied, copy_error) = poll_copy(ui, &slots.copy);

    let revealed = if has_eye {
        ui.data(|d| d.get_temp::<bool>(reveal_id)).unwrap_or(false)
    } else {
        false
    };

    let paste_rect = egui::Rect::from_min_max(
        egui::pos2(outer_rect.min.x + 2.0, outer_rect.min.y + 2.0),
        egui::pos2(outer_rect.min.x + paste_width - 2.0, outer_rect.max.y - 2.0),
    );
    let copy_rect = if copy {
        Some(egui::Rect::from_min_max(
            egui::pos2(outer_rect.min.x + paste_width + 2.0, outer_rect.min.y + 2.0),
            egui::pos2(
                outer_rect.min.x + paste_width + copy_width - 2.0,
                outer_rect.max.y - 2.0,
            ),
        ))
    } else {
        None
    };
    let clear_rect = egui::Rect::from_min_max(
        egui::pos2(outer_rect.max.x - clear_width + 2.0, outer_rect.min.y + 2.0),
        egui::pos2(outer_rect.max.x - 2.0, outer_rect.max.y - 2.0),
    );
    let eye_rect = if has_eye {
        Some(egui::Rect::from_min_max(
            egui::pos2(
                outer_rect.max.x - clear_width - eye_width + 2.0,
                outer_rect.min.y + 2.0,
            ),
            egui::pos2(outer_rect.max.x - clear_width - 2.0, outer_rect.max.y - 2.0),
        ))
    } else {
        None
    };

    let copy_resp = copy_rect.map(|r| ui.interact(r, copy_id, egui::Sense::click()));
    let paste_resp = ui.interact(paste_rect, paste_id, egui::Sense::click());
    let clear_resp = ui.interact(clear_rect, clear_id, egui::Sense::click());
    let eye_resp = eye_rect.map(|r| ui.interact(r, eye_id, egui::Sense::click()));

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
    if let Some(resp) = &eye_resp
        && resp.clicked()
    {
        let _ = ui.data_mut(|d| d.insert_temp(reveal_id, !revealed));
    }

    let _ = ui.painter().vline(
        outer_rect.min.x + paste_width,
        outer_rect.y_range(),
        egui::Stroke::new(1.0, theme.border),
    );
    if copy {
        let _ = ui.painter().vline(
            outer_rect.min.x + left_reserve,
            outer_rect.y_range(),
            egui::Stroke::new(1.0, theme.border),
        );
    }
    let _ = ui.painter().vline(
        outer_rect.max.x - clear_width,
        outer_rect.y_range(),
        egui::Stroke::new(1.0, theme.border),
    );
    if has_eye {
        let _ = ui.painter().vline(
            outer_rect.max.x - right_reserve,
            outer_rect.y_range(),
            egui::Stroke::new(1.0, theme.border),
        );
    }

    let input_rect = egui::Rect::from_min_max(
        egui::pos2(
            outer_rect.min.x + left_reserve + h_padding,
            outer_rect.min.y + 2.0,
        ),
        egui::pos2(
            outer_rect.max.x - right_reserve - h_padding,
            outer_rect.max.y - 2.0,
        ),
    );

    let mut child_ui = ui.new_child(
        egui::UiBuilder::new()
            .max_rect(input_rect)
            .layout(egui::Layout::left_to_right(egui::Align::Center)),
    );

    let text_edit = egui::TextEdit::singleline(text)
        .frame(egui::Frame::NONE)
        .password(password && !revealed)
        .hint_text(&placeholder)
        .text_color(theme.foreground)
        .desired_width(input_rect.width());

    let response = child_ui.add(text_edit);

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

    if let (Some(rect), Some(resp)) = (eye_rect, &eye_resp)
        && ui.is_rect_visible(rect)
    {
        paint_tool_icon(
            ui.painter(),
            rect,
            if revealed {
                LucideIcon::EyeOff
            } else {
                LucideIcon::Eye
            },
            hover_color(&theme, resp.hovered()),
        );
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

    if response.has_focus() {
        paint_focused(ui, outer_rect, &theme);
    }

    respond(response, pasted, copied, cleared, paste_error, copy_error)
}
