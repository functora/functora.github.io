//! Shared clipboard/slot/chrome core for the single-line (`InputPasteClear`)
//! and multi-line (`TextareaPasteClear`) paste-clear fields. Both widgets keep
//! their own geometry and text primitives; everything clipboard-shaped lives
//! here exactly once: channel slots, polling, paste/copy requests, clear
//! semantics, tool-icon colors, outer frame and focus ring.

use std::sync::{Arc, Mutex};

use crate::icons::lucide_icon::LucideIcon;
use crate::theme::shadcn_theme::ShadcnTheme;
use crate::utils::{f32_to_u8_clamped, with_alpha};

pub(crate) type PasteRx = std::sync::mpsc::Receiver<Result<String, crate::error::Error>>;
pub(crate) type CopyRx = std::sync::mpsc::Receiver<Result<(), crate::error::Error>>;

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

pub(crate) struct Slots {
    pub paste: PasteSlot,
    pub copy: CopySlot,
}

/// Loads (or creates) the per-widget clipboard channel slots. `base_id` keeps
/// each widget's temp ids distinct when both share one screen.
pub(crate) fn take_slots(ui: &mut egui::Ui, base_id: egui::Id) -> Slots {
    let slot_id = base_id.with("slot");
    let copy_slot_id = base_id.with("copy_slot");
    let paste = ui
        .data(|d| d.get_temp::<PasteSlot>(slot_id))
        .unwrap_or_else(|| PasteSlot(Arc::new(Mutex::new(None))));
    let _ = ui.data_mut(|d| d.insert_temp(slot_id, paste.clone()));
    let copy = ui
        .data(|d| d.get_temp::<CopySlot>(copy_slot_id))
        .unwrap_or_else(|| CopySlot(Arc::new(Mutex::new(None))));
    let _ = ui.data_mut(|d| d.insert_temp(copy_slot_id, copy.clone()));
    Slots { paste, copy }
}

/// Polls a pending paste read: applies finished text, reports errors, and
/// restores the slot while the read is still in flight.
pub(crate) fn poll_paste(
    ui: &egui::Ui,
    slot: &PasteSlot,
    text: &mut String,
) -> (bool, Option<crate::error::Error>) {
    if let Ok(mut guard) = slot.0.lock()
        && let Some(rx) = guard.take()
    {
        match rx.try_recv() {
            Ok(Ok(incoming)) => {
                incoming.clone_into(text);
                return (true, None);
            }
            Ok(Err(error)) => return (false, Some(error)),
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                *guard = Some(rx);
                ui.ctx().request_repaint();
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                return (
                    false,
                    Some(crate::error::Error::JS(
                        "Clipboard channel disconnected".into(),
                    )),
                );
            }
        }
    }
    (false, None)
}

/// Polls a pending clipboard write.
pub(crate) fn poll_copy(ui: &egui::Ui, slot: &CopySlot) -> (bool, Option<crate::error::Error>) {
    if let Ok(mut guard) = slot.0.lock()
        && let Some(rx) = guard.take()
    {
        match rx.try_recv() {
            Ok(Ok(())) => return (true, None),
            Ok(Err(error)) => return (false, Some(error)),
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                *guard = Some(rx);
                ui.ctx().request_repaint();
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                return (
                    false,
                    Some(crate::error::Error::JS(
                        "Clipboard channel disconnected".into(),
                    )),
                );
            }
        }
    }
    (false, None)
}

/// Whether a paste read is currently in flight (icon renders busy).
pub(crate) fn paste_pending(slot: &PasteSlot) -> bool {
    slot.0.lock().ok().is_some_and(|guard| guard.is_some())
}

/// Whether a clipboard write is currently in flight (icon renders busy).
pub(crate) fn copy_pending(slot: &CopySlot) -> bool {
    slot.0.lock().ok().is_some_and(|guard| guard.is_some())
}

/// Starts an async clipboard read unless one is already in flight.
pub(crate) fn request_paste(ui: &egui::Ui, slot: &PasteSlot) {
    if paste_pending(slot) {
        return;
    }
    let rx = crate::utils::spawn_async(async move { crate::clipboard::read().await });
    if let Ok(mut guard) = slot.0.lock() {
        *guard = Some(rx);
    }
    ui.ctx().request_repaint();
}

/// Starts an async clipboard write of `text` unless one is in flight or the
/// text is empty (copying nothing reports nothing).
pub(crate) fn request_copy(ui: &egui::Ui, slot: &CopySlot, text: &str) {
    if text.is_empty() || copy_pending(slot) {
        return;
    }
    let to_copy = text.to_owned();
    let rx = crate::utils::spawn_async(async move { crate::clipboard::write(to_copy).await });
    if let Ok(mut guard) = slot.0.lock() {
        *guard = Some(rx);
    }
    ui.ctx().request_repaint();
}

/// Resets `text` to `default_value`. Returns whether anything changed, so a
/// click on an already-default field reports no clear.
pub(crate) fn apply_clear(text: &mut String, default_value: &str) -> bool {
    if *text == default_value {
        false
    } else {
        default_value.clone_into(text);
        true
    }
}

/// Idle tool-icon color: foreground on hover, muted otherwise.
pub(crate) fn hover_color(theme: &ShadcnTheme, hovered: bool) -> egui::Color32 {
    if hovered {
        theme.foreground
    } else {
        theme.muted_foreground
    }
}

/// Dims an in-flight tool icon so pending clipboard work reads as busy.
pub(crate) fn pending_color(color: egui::Color32) -> egui::Color32 {
    with_alpha(color, 120)
}

/// Disabled tool-icon color: muted at low alpha.
pub(crate) fn disabled_color(theme: &ShadcnTheme) -> egui::Color32 {
    with_alpha(theme.muted_foreground, 90)
}

pub(crate) fn paint_tool_icon(
    painter: &egui::Painter,
    rect: egui::Rect,
    icon: LucideIcon,
    color: egui::Color32,
) {
    let icon_rect = egui::Rect::from_center_size(rect.center(), egui::vec2(16.0, 16.0));
    crate::icons::paint_icon::paint_icon(painter, icon_rect, &icon, color);
}

/// Paints the shared outer frame: theme background (accent-tinted on hover)
/// with a 1px inside border that switches to the input color on hover.
pub(crate) fn paint_outer(
    ui: &egui::Ui,
    outer_rect: egui::Rect,
    theme: &ShadcnTheme,
    hovered: bool,
) {
    let cr = egui::CornerRadius::same(f32_to_u8_clamped(theme.radius));
    let bg = if hovered {
        crate::paint::interpolate_color::interpolate_color(theme.background, theme.accent, 0.35)
    } else {
        theme.background
    };
    let border = if hovered { theme.input } else { theme.border };
    let _ = ui.painter().rect_filled(outer_rect, cr, bg);
    let _ = ui.painter().rect_stroke(
        outer_rect,
        cr,
        egui::Stroke::new(1.0, border),
        egui::epaint::StrokeKind::Inside,
    );
}

/// Paints the shared focus affordance: ring-color inner stroke plus the
/// 2px-expand focus ring.
pub(crate) fn paint_focused(ui: &egui::Ui, outer_rect: egui::Rect, theme: &ShadcnTheme) {
    let cr = egui::CornerRadius::same(f32_to_u8_clamped(theme.radius));
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

/// Assembles the shared response from one widget's text response plus the
/// clipboard outcomes polled above. A copy error wins over a paste error,
/// matching the historical per-widget polling order.
pub(crate) fn respond(
    response: egui::Response,
    pasted: bool,
    copied: bool,
    cleared: bool,
    paste_error: Option<crate::error::Error>,
    copy_error: Option<crate::error::Error>,
) -> PasteClearResponse {
    PasteClearResponse {
        response,
        pasted,
        copied,
        cleared,
        clipboard_error: copy_error.or(paste_error),
    }
}
