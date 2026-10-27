//! Shared text layout: lays text out inside the box it is given instead of
//! letting it paint past the box and overflow the screen.
//!
//! Every functora-egui widget measures its text through [`layout`] with the
//! inner width of the box it is about to allocate, so wrapped text grows the
//! widget's height rather than its width.

use std::sync::Arc;

/// How text breaks when it does not fit the box it is given.
///
/// The default [`Wrap::Word`] behaves like HTML: text splits on word
/// boundaries, and a word that cannot fit a line of its own is broken between
/// characters (HTML's `overflow-wrap: break-word`), so text never overflows.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Wrap {
    /// Wrap on word boundaries, breaking over-long words as a last resort.
    #[default]
    Word,
    /// Wrap on word boundaries, always allowed to break between characters
    /// (`overflow-wrap: anywhere`).
    Any,
    /// Keep a single line and let the text extend past the box
    /// (`white-space: nowrap`).
    Nowrap,
    /// Keep a single line and elide the rest with `…`
    /// (`text-overflow: ellipsis`).
    Truncate,
}

/// Resolves the wrap mode to use for text in `ui`.
///
/// A widget-level override wins, otherwise the explicit app-wide
/// [`egui::Style::wrap_mode`] is honored, otherwise text wraps on words.
#[must_use]
pub fn resolve(ui: &egui::Ui, wrap: Option<Wrap>) -> Wrap {
    wrap.unwrap_or_else(|| ui.style().wrap_mode.map_or(Wrap::Word, Into::into))
}

impl From<egui::TextWrapMode> for Wrap {
    fn from(mode: egui::TextWrapMode) -> Self {
        match mode {
            egui::TextWrapMode::Extend => Self::Nowrap,
            egui::TextWrapMode::Wrap => Self::Word,
            egui::TextWrapMode::Truncate => Self::Truncate,
        }
    }
}

/// Lays `text` out so that no row is wider than `max_width`.
///
/// [`Wrap::Word`] lays the text out on word boundaries first and, only if a
/// single word still does not fit, re-lays it out breaking between characters,
/// so the returned galley never exceeds `max_width` (both passes are
/// memoized by egui's font cache).
///
/// `wrap` is a widget-level override; when `None` the app-wide
/// [`egui::Style::wrap_mode`] is honored and text wraps on words by default.
pub fn layout(
    ui: &egui::Ui,
    text: impl Into<String>,
    font_id: egui::FontId,
    color: egui::Color32,
    max_width: f32,
    wrap: Option<Wrap>,
) -> Arc<egui::Galley> {
    resolved_layout(
        ui,
        text.into(),
        font_id,
        color,
        max_width.max(0.0),
        resolve(ui, wrap),
    )
}

fn resolved_layout(
    ui: &egui::Ui,
    text: String,
    font_id: egui::FontId,
    color: egui::Color32,
    max_width: f32,
    wrap: Wrap,
) -> Arc<egui::Galley> {
    match wrap {
        Wrap::Nowrap => ui.painter().layout_no_wrap(text, font_id, color),
        Wrap::Word => {
            let galley = row_layout(
                ui,
                text.clone(),
                font_id.clone(),
                color,
                max_width,
                false,
                usize::MAX,
            );
            if !galley.size().x.is_finite() || galley.size().x <= max_width {
                galley
            } else {
                row_layout(ui, text, font_id, color, max_width, true, usize::MAX)
            }
        }
        Wrap::Any => row_layout(ui, text, font_id, color, max_width, true, usize::MAX),
        Wrap::Truncate => row_layout(ui, text, font_id, color, max_width, true, 1),
    }
}

fn row_layout(
    ui: &egui::Ui,
    text: impl Into<String>,
    font_id: egui::FontId,
    color: egui::Color32,
    max_width: f32,
    break_anywhere: bool,
    max_rows: usize,
) -> Arc<egui::Galley> {
    let mut job = egui::text::LayoutJob::simple(text.into(), font_id, color, max_width);
    job.wrap.break_anywhere = break_anywhere;
    job.wrap.max_rows = max_rows;
    ui.fonts_mut(|fonts| fonts.layout_job(job))
}

/// Width `text` takes on one unbroken line, e.g. to size a container around
/// labels before their [`egui::Ui`] exists.
#[must_use]
pub fn natural_width(ctx: &egui::Context, text: impl Into<String>, font_id: egui::FontId) -> f32 {
    ctx.fonts_mut(|fonts| fonts.layout_no_wrap(text.into(), font_id, egui::Color32::WHITE))
        .size()
        .x
}

/// Height a box needs so that `galley` fits inside it.
///
/// Keeps the vertical padding a single-line box of `min_height` has, so a box
/// grows only when the text actually wraps onto more lines.
#[must_use]
pub fn box_height(galley: &egui::Galley, min_height: f32) -> f32 {
    let line_height = galley.rows.first().map_or(0.0, |row| row.height());
    let padding_y = ((min_height - line_height) * 0.5).max(0.0);
    min_height.max(galley.size().y + padding_y * 2.0)
}

/// Box height for the intrinsic (non-wrapped) text of `galley`.
///
/// The counterpart of [`box_height`] for intrinsic sizes: the height the box
/// would have if the text never wrapped or truncated.
#[must_use]
pub fn intrinsic_box_height(galley: &egui::Galley, min_height: f32) -> f32 {
    let line_height = galley.intrinsic_size().y;
    let padding_y = ((min_height - line_height) * 0.5).max(0.0);
    min_height.max(line_height + padding_y * 2.0)
}
