use std::sync::{Arc, Mutex};

use crate::icons::lucide_icon::LucideIcon;
use crate::utils::f32_to_u8_clamped;

type PasteRx = std::sync::mpsc::Receiver<Result<String, crate::error::Error>>;

#[derive(Clone)]
pub(crate) struct PasteSlot(pub Arc<Mutex<Option<PasteRx>>>);

#[derive(Debug)]
pub struct PasteClearResponse {
    pub response: egui::Response,
    pub pasted: bool,
    pub cleared: bool,
    pub clipboard_error: Option<crate::error::Error>,
}

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
    } = widget;
    let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let h_padding: f32 = 10.0;
    let v_padding: f32 = 8.0;
    let width = ui.available_width();
    let cr = egui::CornerRadius::same(f32_to_u8_clamped(theme.radius));

    let desired = egui::vec2(width, min_height);
    let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::hover());
    let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

    let mut bg =
        crate::paint::interpolate_color::interpolate_color(theme.background, theme.muted, 0.4);
    if outer_hovered {
        bg = crate::paint::interpolate_color::interpolate_color(bg, theme.accent, 0.35);
    }
    let _ = ui.painter().rect_filled(outer_rect, cr, bg);
    let _ = ui.painter().rect_stroke(
        outer_rect,
        cr,
        egui::Stroke::new(
            1.0,
            if outer_hovered {
                theme.input
            } else {
                theme.border
            },
        ),
        egui::epaint::StrokeKind::Inside,
    );

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

    let ptr = std::ptr::from_ref::<String>(text).cast::<()>();
    let base_id = egui::Id::new(ptr).with(ui.id());
    let slot_id = base_id.with("slot");
    let paste_id = base_id.with("paste_btn");
    let clear_id = base_id.with("clear_btn");

    let slot = ui
        .data(|d| d.get_temp::<PasteSlot>(slot_id))
        .unwrap_or_else(|| PasteSlot(Arc::new(Mutex::new(None))));
    let _ = ui.data_mut(|d| d.insert_temp(slot_id, slot.clone()));

    let mut pasted = false;
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

    let paste_rect = egui::Rect::from_min_max(
        egui::pos2(toolbar_rect.min.x + 4.0, toolbar_rect.min.y + 4.0),
        egui::pos2(toolbar_rect.min.x + 32.0, toolbar_rect.max.y - 4.0),
    );
    let clear_rect = egui::Rect::from_min_max(
        egui::pos2(toolbar_rect.max.x - 32.0, toolbar_rect.min.y + 4.0),
        egui::pos2(toolbar_rect.max.x - 4.0, toolbar_rect.max.y - 4.0),
    );

    let paste_resp = ui.interact(paste_rect, paste_id, egui::Sense::click());
    let clear_resp = ui.interact(clear_rect, clear_id, egui::Sense::click());

    let is_pending = slot.0.lock().ok().is_some_and(|g| g.is_some());

    if paste_resp.clicked() && !is_pending {
        let rx = crate::utils::spawn_async(async move { crate::clipboard::read().await });
        if let Ok(mut guard) = slot.0.lock() {
            *guard = Some(rx);
        }
        ui.ctx().request_repaint();
    }

    let mut cleared = false;
    if clear_resp.clicked() && *text != default_value {
        default_value.clone_into(text);
        cleared = true;
    }

    let _ = ui.painter().hline(
        toolbar_rect.min.x + h_padding..=toolbar_rect.max.x - h_padding,
        toolbar_rect.max.y,
        egui::Stroke::new(1.0, theme.border),
    );

    if ui.is_rect_visible(paste_rect) {
        let icon_rect = egui::Rect::from_center_size(paste_rect.center(), egui::vec2(16.0, 16.0));
        let paste_color = if is_pending {
            egui::Color32::from_rgba_unmultiplied(
                theme.muted_foreground.r(),
                theme.muted_foreground.g(),
                theme.muted_foreground.b(),
                120,
            )
        } else if paste_resp.hovered() {
            theme.foreground
        } else {
            theme.muted_foreground
        };
        crate::icons::paint_icon::paint_icon(ui.painter(), icon_rect, &paste_icon, paste_color);
    }

    if ui.is_rect_visible(clear_rect) {
        let icon_rect = egui::Rect::from_center_size(clear_rect.center(), egui::vec2(16.0, 16.0));
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
        crate::icons::paint_icon::paint_icon(ui.painter(), icon_rect, &icon, clear_color);
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
                .desired_rows(3);
            inner_ui.add(text_edit)
        });

    let response = scroll_resp.inner;

    if outer_response.clicked() && !response.has_focus() {
        response.request_focus();
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
        cleared,
        clipboard_error,
    }
}
