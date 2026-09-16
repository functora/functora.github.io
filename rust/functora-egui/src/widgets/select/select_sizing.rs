//! Shared content-aware sizing for dropdown triggers and popups.
//!
//! The trigger grows to fit its longest label when the container has space,
//! capped by the available width so it never overflows. Explicit `.width(x)`
//! overrides are still honoured but clamped to the container.

use crate::responsive::responsive_ext::ResponsiveExt;

pub(crate) const TRIGGER_FONT_SIZE: f32 = 14.0;
const TRIGGER_BREATHING: f32 = 4.0;
const POPUP_ITEM_PADDING: f32 = 32.0;
const POPUP_CONTENT_FLOOR: f32 = 144.0;

pub(crate) fn text_width(ui: &egui::Ui, text: &str) -> f32 {
    ui.painter()
        .layout_no_wrap(
            text.to_owned(),
            egui::FontId::proportional(TRIGGER_FONT_SIZE),
            egui::Color32::PLACEHOLDER,
        )
        .size()
        .x
}

pub(crate) fn max_text_width<'a>(ui: &egui::Ui, texts: impl IntoIterator<Item = &'a str>) -> f32 {
    texts
        .into_iter()
        .map(|text| text_width(ui, text))
        .fold(0.0, f32::max)
}

pub(crate) fn trigger_width<'a>(
    ui: &egui::Ui,
    width_opt: Option<f32>,
    texts: impl IntoIterator<Item = &'a str>,
    h_padding: f32,
    chevron_reserve: f32,
    floor: f32,
) -> f32 {
    let avail = ui.available_width().max(0.0);
    width_opt.map_or_else(
        || {
            if ui.on_mobile() {
                avail
            } else {
                let measured = max_text_width(ui, texts)
                    + h_padding * 2.0
                    + chevron_reserve
                    + TRIGGER_BREATHING;
                crate::utils::clamp_overlay_width(measured, floor, avail)
            }
        },
        |explicit| explicit.min(avail).max(0.0),
    )
}

pub(crate) fn popup_width<'a>(
    ui: &egui::Ui,
    trigger: f32,
    texts: impl IntoIterator<Item = &'a str>,
    floor_mobile: f32,
    floor_desktop: f32,
) -> f32 {
    let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
    let raw = max_text_width(ui, texts) + POPUP_ITEM_PADDING;
    let content_w = raw.max(POPUP_CONTENT_FLOOR);
    let screen_w = ui.ctx().input(|i| i.viewport_rect().width());
    let screen_cap = (screen_w - 2.0 * spacing.page_padding - 16.0).max(0.0);
    if spacing.is_mobile() {
        crate::utils::clamp_overlay_width(trigger.max(content_w), floor_mobile, screen_cap)
    } else {
        crate::utils::clamp_overlay_width(content_w.max(trigger), floor_desktop, screen_cap)
    }
}
