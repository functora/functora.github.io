use std::sync::{Arc, Mutex};

use crate::icons::lucide_icon::LucideIcon;
use crate::utils::f32_to_u8_clamped;

type PasteRx = std::sync::mpsc::Receiver<Result<String, crate::error::Error>>;
type CopyRx = std::sync::mpsc::Receiver<Result<(), crate::error::Error>>;

#[derive(Clone)]
pub(crate) struct PasteSlot(pub Arc<Mutex<Option<PasteRx>>>);

#[derive(Clone)]
pub(crate) struct CopySlot(pub Arc<Mutex<Option<CopyRx>>>);

#[derive(Debug)]
pub struct PasteClearResponse {
    pub response: egui::Response,
    pub pasted: bool,
    pub copied: bool,
    pub cleared: bool,
    pub clipboard_error: Option<crate::error::Error>,
}

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
    let cr = egui::CornerRadius::same(f32_to_u8_clamped(theme.radius));
    let h_padding = spacing.touch_padding;

    let width = ui.available_width();
    let desired = egui::vec2(width, height);
    let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::hover());
    let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

    let bg = if outer_hovered {
        crate::paint::interpolate_color::interpolate_color(theme.background, theme.accent, 0.35)
    } else {
        theme.background
    };
    let border_color = if outer_hovered {
        theme.input
    } else {
        theme.border
    };
    let _ = ui.painter().rect_filled(outer_rect, cr, bg);
    let _ = ui.painter().rect_stroke(
        outer_rect,
        cr,
        egui::Stroke::new(1.0, border_color),
        egui::epaint::StrokeKind::Inside,
    );

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
    let slot_id = base_id.with("slot");
    let paste_id = base_id.with("paste_btn");
    let clear_id = base_id.with("clear_btn");
    let reveal_id = base_id.with("reveal");
    let eye_id = base_id.with("eye_btn");
    let copy_slot_id = base_id.with("copy_slot");
    let copy_id = base_id.with("copy_btn");

    let slot = ui
        .data(|d| d.get_temp::<PasteSlot>(slot_id))
        .unwrap_or_else(|| PasteSlot(Arc::new(Mutex::new(None))));
    let _ = ui.data_mut(|d| d.insert_temp(slot_id, slot.clone()));

    let copy_slot = ui
        .data(|d| d.get_temp::<CopySlot>(copy_slot_id))
        .unwrap_or_else(|| CopySlot(Arc::new(Mutex::new(None))));
    let _ = ui.data_mut(|d| d.insert_temp(copy_slot_id, copy_slot.clone()));

    let mut pasted = false;
    let mut copied = false;
    let mut clipboard_error: Option<crate::error::Error> = None;

    if let Ok(mut guard) = slot.0.lock()
        && let Some(rx) = guard.take()
    {
        match rx.try_recv() {
            Ok(Ok(txt)) => {
                txt.clone_into(text);
                pasted = true;
            }
            Ok(Err(e)) => {
                clipboard_error = Some(e);
            }
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                *guard = Some(rx);
                ui.ctx().request_repaint();
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                clipboard_error = Some(crate::error::Error::JS(
                    "Clipboard channel disconnected".into(),
                ));
            }
        }
    }

    if let Ok(mut guard) = copy_slot.0.lock()
        && let Some(rx) = guard.take()
    {
        match rx.try_recv() {
            Ok(Ok(())) => {
                copied = true;
            }
            Ok(Err(e)) => {
                clipboard_error = Some(e);
            }
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                *guard = Some(rx);
                ui.ctx().request_repaint();
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                clipboard_error = Some(crate::error::Error::JS(
                    "Clipboard channel disconnected".into(),
                ));
            }
        }
    }

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

    let is_paste_pending = slot.0.lock().ok().is_some_and(|g| g.is_some());
    let is_copy_pending = copy_slot.0.lock().ok().is_some_and(|g| g.is_some());

    if paste_resp.clicked() && !is_paste_pending {
        let rx = crate::utils::spawn_async(async move { crate::clipboard::read().await });
        if let Ok(mut guard) = slot.0.lock() {
            *guard = Some(rx);
        }
        ui.ctx().request_repaint();
    }

    if let Some(resp) = &copy_resp
        && resp.clicked()
        && !is_copy_pending
        && !text.is_empty()
    {
        let to_copy = text.clone();
        let rx = crate::utils::spawn_async(async move { crate::clipboard::write(to_copy).await });
        if let Ok(mut guard) = copy_slot.0.lock() {
            *guard = Some(rx);
        }
        ui.ctx().request_repaint();
    }

    let mut cleared = false;
    if clear_resp.clicked() && *text != default_value {
        default_value.clone_into(text);
        cleared = true;
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
        let icon_rect = egui::Rect::from_center_size(rect.center(), egui::vec2(16.0, 16.0));
        let empty = text.is_empty();
        let base_color = if empty {
            egui::Color32::from_rgba_unmultiplied(
                theme.muted_foreground.r(),
                theme.muted_foreground.g(),
                theme.muted_foreground.b(),
                90,
            )
        } else if resp.hovered() {
            theme.foreground
        } else {
            theme.muted_foreground
        };
        let display_color = if is_copy_pending {
            egui::Color32::from_rgba_unmultiplied(
                base_color.r(),
                base_color.g(),
                base_color.b(),
                120,
            )
        } else {
            base_color
        };
        crate::icons::paint_icon::paint_icon(ui.painter(), icon_rect, &copy_icon, display_color);
    }

    if ui.is_rect_visible(paste_rect) {
        let paste_icon_rect =
            egui::Rect::from_center_size(paste_rect.center(), egui::vec2(16.0, 16.0));
        let base_paste_color = if paste_resp.hovered() {
            theme.foreground
        } else {
            theme.muted_foreground
        };
        let display_paste_color = if is_paste_pending {
            egui::Color32::from_rgba_unmultiplied(
                base_paste_color.r(),
                base_paste_color.g(),
                base_paste_color.b(),
                120,
            )
        } else {
            base_paste_color
        };
        crate::icons::paint_icon::paint_icon(
            ui.painter(),
            paste_icon_rect,
            &paste_icon,
            display_paste_color,
        );
    }

    if let (Some(rect), Some(resp)) = (eye_rect, &eye_resp)
        && ui.is_rect_visible(rect)
    {
        let icon_rect = egui::Rect::from_center_size(rect.center(), egui::vec2(16.0, 16.0));
        let eye_color = if resp.hovered() {
            theme.foreground
        } else {
            theme.muted_foreground
        };
        crate::icons::paint_icon::paint_icon(
            ui.painter(),
            icon_rect,
            if revealed {
                &LucideIcon::EyeOff
            } else {
                &LucideIcon::Eye
            },
            eye_color,
        );
    }

    if ui.is_rect_visible(clear_rect) {
        let clear_icon_rect =
            egui::Rect::from_center_size(clear_rect.center(), egui::vec2(16.0, 16.0));
        let clear_enabled = *text != default_value;
        let clear_color = if !clear_enabled {
            egui::Color32::from_rgba_unmultiplied(
                theme.muted_foreground.r(),
                theme.muted_foreground.g(),
                theme.muted_foreground.b(),
                90,
            )
        } else if clear_resp.hovered() {
            theme.foreground
        } else {
            theme.muted_foreground
        };
        let icon = if clear_enabled {
            clear_icon
        } else {
            LucideIcon::X
        };
        crate::icons::paint_icon::paint_icon(ui.painter(), clear_icon_rect, &icon, clear_color);
    }

    if response.has_focus() {
        let _ = ui.painter().rect_stroke(
            outer_rect,
            cr,
            egui::Stroke::new(1.0, theme.ring),
            egui::epaint::StrokeKind::Inside,
        );
        crate::paint::paint_focus_ring::paint_focus_ring(
            ui.painter(),
            outer_rect,
            theme.radius,
            theme.ring,
        );
    }

    PasteClearResponse {
        response,
        pasted,
        copied,
        cleared,
        clipboard_error,
    }
}
