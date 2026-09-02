use crate::error::Error;
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
use crate::widgets::qr_scanner::qr_scanner_state::QrScannerState;
use std::sync::Arc;

type ScanFn = Arc<dyn Fn(String) + Send + Sync>;
type ErrorFn = Arc<dyn Fn(&Error) + Send + Sync>;

/// Automatic QR scanner: live camera feed plus rate-limited decoding.
///
/// By default the scanner starts on first `show` (firing the platform
/// permission prompt), decodes continuously at `decode_fps`, and stops after
/// the first code. `.continuous(true)` keeps scanning and fires `on_scan`
/// for every distinct code, rate-limited by `.dedupe_ms`.
#[must_use]
pub struct QrScanner {
    desired_size: egui::Vec2,
    fps: f32,
    decode_fps: f32,
    dedupe_ms: u64,
    continuous: bool,
    auto_start: bool,
    on_scan: Option<ScanFn>,
    on_error: Option<ErrorFn>,
}

impl Default for QrScanner {
    fn default() -> Self {
        Self {
            desired_size: egui::vec2(320.0, 240.0),
            fps: 15.0,
            decode_fps: 5.0,
            dedupe_ms: 1500,
            continuous: false,
            auto_start: true,
            on_scan: None,
            on_error: None,
        }
    }
}

impl QrScanner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn desired_size(mut self, size: egui::Vec2) -> Self {
        self.desired_size = size;
        self
    }

    /// Live preview capture rate (1..=60 fps, default 15).
    pub fn fps(mut self, fps: f32) -> Self {
        self.fps = fps;
        self
    }

    /// Decode attempts per second (default 5; throttled independently of the
    /// preview so decoding cost never caps video smoothness).
    pub fn decode_fps(mut self, decode_fps: f32) -> Self {
        self.decode_fps = decode_fps;
        self
    }

    /// Minimum delay before the same code fires `on_scan` again in
    /// continuous mode (default 1500 ms).
    pub fn dedupe_ms(mut self, ms: u64) -> Self {
        self.dedupe_ms = ms;
        self
    }

    /// Keep scanning after a hit and fire `on_scan` for every distinct code.
    pub fn continuous(mut self, yes: bool) -> Self {
        self.continuous = yes;
        self
    }

    /// Begin scanning automatically on the first `show`.
    pub fn auto_start(mut self, yes: bool) -> Self {
        self.auto_start = yes;
        self
    }

    pub fn on_scan(mut self, f: impl Fn(String) + Send + Sync + 'static) -> Self {
        self.on_scan = Some(Arc::new(f));
        self
    }

    pub fn on_error(mut self, f: impl Fn(&Error) + Send + Sync + 'static) -> Self {
        self.on_error = Some(Arc::new(f));
        self
    }

    pub fn show(self, ui: &mut egui::Ui, state: &mut QrScannerState) -> egui::Response {
        let theme = ui.ctx().shadcn_theme();
        state.configure(
            self.fps,
            self.decode_fps,
            self.dedupe_ms,
            self.continuous,
            self.on_scan.clone(),
            self.on_error.clone(),
        );
        if self.auto_start && !state.is_scanning() && state.error().is_none() {
            let _ = state.start(ui.ctx());
        }
        egui::Frame::new()
            .fill(theme.card)
            .stroke(egui::Stroke::new(1.0, theme.border))
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                theme.radius,
            )))
            .inner_margin(egui::Margin::same(12))
            .show(ui, |inner| {
                let desired_size = self.desired_size;
                if let Some((rgba, w, h)) = state.drain_rgba() {
                    state.store_preview(inner.ctx(), &rgba, w, h);
                }
                let running = state.is_scanning();
                match state.preview_texture() {
                    Some(tex) => {
                        let id = tex.id();
                        let _ = inner.add(
                            egui::Image::new((id, desired_size))
                                .corner_radius(egui::CornerRadius::same(8)),
                        );
                    }
                    None => placeholder(inner, desired_size, &theme, "Starting camera…"),
                }
                if running {
                    inner.ctx().request_repaint();
                }
                if let Some(err) = state.error() {
                    inner.add_space(6.0);
                    fire_on_error(state, &err);
                    let _ = inner.label(
                        egui::RichText::new(err.to_string())
                            .color(theme.destructive)
                            .size(12.0),
                    );
                }
                if let Some(txt) = state.decoded() {
                    inner.add_space(4.0);
                    let _ = inner.label(
                        egui::RichText::new(format!("Decoded: {txt}"))
                            .color(theme.foreground)
                            .size(13.0)
                            .strong(),
                    );
                }
                inner.add_space(8.0);
                controls_row(inner, state);
            })
            .response
    }
}

fn placeholder(
    ui: &mut egui::Ui,
    size: egui::Vec2,
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    text: &str,
) {
    let (rect, _) = ui.allocate_exact_size(size, egui::Sense::hover());
    if ui.is_rect_visible(rect) {
        let _ = ui
            .painter()
            .rect_filled(rect, egui::CornerRadius::same(8), theme.muted);
        let _ = ui.painter().text(
            rect.center(),
            egui::Align2::CENTER_CENTER,
            text,
            egui::FontId::proportional(13.0),
            theme.muted_foreground,
        );
    }
}

fn controls_row(ui: &mut egui::Ui, state: &mut QrScannerState) {
    let _ = ui.horizontal(|row| {
        let running = state.is_scanning();
        let label = if running { "Stop" } else { "Start" };
        if row
            .add(crate::Button::new(label).variant(crate::ButtonVariant::Secondary))
            .clicked()
        {
            if running {
                state.stop();
            } else {
                state.clear_error();
                let ctx = row.ctx().clone();
                let _ = state.start(&ctx);
            }
        }
        #[cfg(any(target_arch = "wasm32", not(target_os = "android")))]
        if row
            .add(crate::Button::new("Pick Image").variant(crate::ButtonVariant::Outline))
            .clicked()
        {
            spawn_pick_image(state, row.ctx());
        }
        if row
            .add(crate::Button::new("Clear").variant(crate::ButtonVariant::Ghost))
            .clicked()
        {
            state.clear_error();
            state.clear_decoded();
        }
    });
}

fn fire_on_error(state: &QrScannerState, err: &Error) {
    if let Some(cb) = state.on_error_callback() {
        cb(err);
    }
}

#[cfg(any(target_arch = "wasm32", not(target_os = "android")))]
fn spawn_pick_image(state: &mut QrScannerState, ui_ctx: &egui::Context) {
    let slots = state.pick_slots();
    let ctx = (*ui_ctx).clone();
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        wasm_bindgen_futures::spawn_local(async move {
            match crate::files::pick_files(false).await {
                Ok(files) => {
                    for (_, data) in files {
                        if let Some(txt) = decode_bytes(&data) {
                            slots.set_decoded(txt);
                            ctx.request_repaint();
                            return;
                        }
                    }
                    slots.set_error_message("No QR found in image");
                    ctx.request_repaint();
                }
                Err(e) => {
                    slots.set_error(&e);
                    ctx.request_repaint();
                }
            }
        });
    }
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        let _ = std::thread::spawn(move || {
            let picked = rfd::FileDialog::new()
                .add_filter("Image", &["png", "jpg", "jpeg"])
                .pick_file();
            if let Some(path) = picked {
                let data = std::fs::read(&path).unwrap_or_default();
                if let Some(txt) = decode_bytes(&data) {
                    slots.set_decoded(txt);
                } else {
                    slots.set_error_message("No QR found in image");
                }
                ctx.request_repaint();
            }
        });
    }
}

#[cfg(all(
    feature = "camera",
    feature = "qr",
    any(target_arch = "wasm32", not(target_os = "android"))
))]
fn decode_bytes(data: &[u8]) -> Option<String> {
    let img = image::load_from_memory(data).ok()?;
    let rgba = img.to_rgba8();
    let (w, h) = (rgba.width(), rgba.height());
    let raw = rgba.into_raw();
    crate::qr::decode_qr_rgba(&raw, w, h)
}

#[cfg(all(
    not(all(feature = "camera", feature = "qr")),
    any(target_arch = "wasm32", not(target_os = "android"))
))]
fn decode_bytes(_data: &[u8]) -> Option<String> {
    None
}
