use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
use crate::widgets::qr_scanner::qr_scanner_state::QrScannerState;

#[must_use]
pub struct QrScanner {
    desired_size: egui::Vec2,
}

impl Default for QrScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl QrScanner {
    pub fn new() -> Self {
        Self {
            desired_size: egui::vec2(320.0, 240.0),
        }
    }

    pub fn desired_size(mut self, size: egui::Vec2) -> Self {
        self.desired_size = size;
        self
    }

    pub fn show(self, ui: &mut egui::Ui, state: &mut QrScannerState) -> egui::Response {
        let theme = ui.ctx().shadcn_theme();
        let outer = egui::Frame::new()
            .fill(theme.card)
            .stroke(egui::Stroke::new(1.0, theme.border))
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                theme.radius,
            )))
            .inner_margin(egui::Margin::same(12))
            .show(ui, |inner_ui| {
                Self::content(inner_ui, state, self.desired_size, &theme);
            });
        outer.response
    }

    fn content(
        ui: &mut egui::Ui,
        state: &mut QrScannerState,
        desired_size: egui::Vec2,
        theme: &crate::theme::shadcn_theme::ShadcnTheme,
    ) {
        let is_scanning = state.is_scanning();
        let error = state.error();
        let decoded = state.decoded();

        if let Some((rgba, w, h)) = state.take_latest_rgba() {
            let image = egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
            let tex = ui
                .ctx()
                .load_texture("qr-preview", image, egui::TextureOptions::LINEAR);
            *state.texture_mut() = Some(tex);
        }

        if is_scanning {
            if let Some(tex) = state.texture_mut().clone() {
                let _ = ui.add(
                    egui::Image::new((tex.id(), desired_size))
                        .corner_radius(egui::CornerRadius::same(8)),
                );
            } else {
                let (rect, _) = ui.allocate_exact_size(desired_size, egui::Sense::hover());
                if ui.is_rect_visible(rect) {
                    let _ =
                        ui.painter()
                            .rect_filled(rect, egui::CornerRadius::same(8), theme.muted);
                    let _ = ui.painter().text(
                        rect.center(),
                        egui::Align2::CENTER_CENTER,
                        "Starting camera…",
                        egui::FontId::proportional(13.0),
                        theme.muted_foreground,
                    );
                }
                ui.ctx().request_repaint();
            }
        } else if let Some(tex) = state.texture_mut().clone() {
            let _ = ui.add(
                egui::Image::new((tex.id(), desired_size))
                    .corner_radius(egui::CornerRadius::same(8)),
            );
        } else {
            let (rect, _) = ui.allocate_exact_size(desired_size, egui::Sense::hover());
            if ui.is_rect_visible(rect) {
                let _ = ui
                    .painter()
                    .rect_filled(rect, egui::CornerRadius::same(8), theme.muted);
                let _ = ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    "No preview",
                    egui::FontId::proportional(13.0),
                    theme.muted_foreground,
                );
            }
        }

        ui.add_space(8.0);

        if let Some(err) = error {
            let _ = ui.label(
                egui::RichText::new(err.to_string())
                    .color(theme.destructive)
                    .size(12.0),
            );
            ui.add_space(4.0);
        }

        if let Some(txt) = decoded {
            let _ = ui.label(
                egui::RichText::new(format!("Decoded: {txt}"))
                    .color(theme.foreground)
                    .size(13.0)
                    .strong(),
            );
            ui.add_space(4.0);
        }

        let ctx_clone = ui.ctx().clone();
        let _ = ui.horizontal(|row| {
            if is_scanning {
                if row
                    .add(crate::Button::new("Stop").variant(crate::ButtonVariant::Secondary))
                    .clicked()
                {
                    state.stop();
                    let inner = state.inner_arc();
                    if let Ok(mut g) = inner.lock() {
                        g.scanning = false;
                    }
                    ctx_clone.request_repaint();
                    spawn_stop();
                }
            } else {
                let start_label = if state.decoded().is_some() {
                    "Scan again"
                } else {
                    "Start Camera"
                };
                if row
                    .add(crate::Button::new(start_label).variant(crate::ButtonVariant::Default))
                    .clicked()
                {
                    state.clear_error();
                    state.clear_decoded();
                    if let Some(guard) = state.in_flight().claim() {
                        state.set_scanning(true);
                        let epoch = state.bump_epoch();
                        let inner = state.inner_arc();
                        spawn_camera_loop(&ctx_clone, &inner, epoch, guard);
                    }
                }
            }

            if row
                .add(crate::Button::new("Pick Image").variant(crate::ButtonVariant::Outline))
                .clicked()
            {
                let inner = state.inner_arc();
                spawn_pick_image(&inner, &ctx_clone);
            }

            if row
                .add(crate::Button::new("Clear").variant(crate::ButtonVariant::Ghost))
                .clicked()
            {
                state.clear_error();
                state.clear_decoded();
                state.stop();
                *state.texture_mut() = None;
            }
        });
    }
}

fn spawn_stop() {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        wasm_bindgen_futures::spawn_local(async move {
            let _ = crate::camera::stop_camera().await;
        });
    }
    #[cfg(target_os = "android")]
    {
        crate::camera::stop_capture_worker();
    }
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        crate::camera::stop_capture_worker();
    }
}

fn spawn_camera_loop(
    ctx: &egui::Context,
    inner: &std::sync::Arc<std::sync::Mutex<crate::widgets::qr_scanner::qr_scanner_state::QrInner>>,
    epoch: u64,
    guard: crate::in_flight::InFlightGuard,
) {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        let ctx = (*ctx).clone();
        let inner = std::sync::Arc::clone(inner);
        wasm_bindgen_futures::spawn_local(async move {
            if let Err(e) = crate::camera::check_camera().await {
                if crate::widgets::qr_scanner::qr_scanner_state::camera_epoch() == epoch {
                    if let Ok(mut g) = inner.lock() {
                        g.error = Some(std::sync::Arc::new(map_camera_error(&e)));
                        g.scanning = false;
                    }
                }
                ctx.request_repaint();
                return;
            }
            if crate::widgets::qr_scanner::qr_scanner_state::camera_epoch() != epoch {
                return;
            }
            if let Err(e) = crate::camera::start_camera().await {
                if crate::widgets::qr_scanner::qr_scanner_state::camera_epoch() == epoch {
                    if let Ok(mut g) = inner.lock() {
                        g.error = Some(std::sync::Arc::new(map_camera_error(&e)));
                        g.scanning = false;
                    }
                }
                ctx.request_repaint();
                return;
            }
            crate::camera::begin_capture_session();
            loop {
                let scanning = inner.lock().ok().is_some_and(|g| g.scanning);
                let decoded = inner.lock().ok().and_then(|g| g.decoded.clone()).is_some();
                if !scanning || decoded {
                    break;
                }
                if crate::widgets::qr_scanner::qr_scanner_state::camera_epoch() != epoch {
                    break;
                }
                match crate::camera::capture_frame().await {
                    Ok(frame) => {
                        if let Some(rgba) = frame.preview_rgba.clone() {
                            if let Ok(mut g) = inner.lock() {
                                if g.epoch == epoch {
                                    g.latest_rgba = Some((rgba, frame.width, frame.height));
                                }
                            }
                            ctx.request_repaint();
                        }
                        #[cfg(feature = "qr")]
                        {
                            if let Some(txt) =
                                crate::qr::decode_qr_luma(&frame.data, frame.width, frame.height)
                            {
                                if let Ok(mut g) = inner.lock() {
                                    if g.epoch == epoch {
                                        g.decoded = Some(txt);
                                        g.scanning = false;
                                    }
                                }
                                ctx.request_repaint();
                                break;
                            }
                        }
                        #[cfg(not(feature = "qr"))]
                        {
                            let _ = &frame;
                        }
                    }
                    Err(e) => {
                        if let Ok(mut g) = inner.lock() {
                            if g.epoch == epoch {
                                g.error = Some(std::sync::Arc::new(map_camera_error(&e)));
                                g.scanning = false;
                            }
                        }
                        ctx.request_repaint();
                        break;
                    }
                }
                let _ = crate::camera::sleep(300).await;
            }
            if crate::widgets::qr_scanner::qr_scanner_state::camera_epoch() == epoch {
                let _ = crate::camera::stop_camera().await;
            }
            crate::camera::stop_capture_worker();
            drop(guard);
            ctx.request_repaint();
        });
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "web")))]
    {
        let _ = (ctx, inner, epoch);
        drop(guard);
    }
}

fn spawn_pick_image(
    inner: &std::sync::Arc<std::sync::Mutex<crate::widgets::qr_scanner::qr_scanner_state::QrInner>>,
    ctx: &egui::Context,
) {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        let inner_w = std::sync::Arc::clone(inner);
        let ctx_w = (*ctx).clone();
        wasm_bindgen_futures::spawn_local(async move {
            match crate::files::pick_files(false).await {
                Ok(files) => {
                    for (_, data) in files {
                        if let Some(txt) = decode_bytes(&data) {
                            if let Ok(mut g) = inner_w.lock() {
                                g.decoded = Some(txt);
                                g.error = None;
                            }
                            ctx_w.request_repaint();
                            return;
                        }
                    }
                    if let Ok(mut g) = inner_w.lock() {
                        g.error = Some(std::sync::Arc::new(crate::error::Error::JS(
                            "No QR found in image".into(),
                        )));
                    }
                    ctx_w.request_repaint();
                }
                Err(e) => {
                    if let Ok(mut g) = inner_w.lock() {
                        g.error = Some(std::sync::Arc::new(e));
                    }
                    ctx_w.request_repaint();
                }
            }
        });
    }
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        let inner_d = std::sync::Arc::clone(inner);
        let ctx_d = (*ctx).clone();
        let _ = std::thread::spawn(move || {
            let picked = rfd::FileDialog::new()
                .add_filter("Image", &["png", "jpg", "jpeg"])
                .pick_file();
            if let Some(path) = picked {
                let data = std::fs::read(&path).unwrap_or_default();
                if let Some(txt) = decode_bytes(&data) {
                    if let Ok(mut g) = inner_d.lock() {
                        g.decoded = Some(txt);
                        g.error = None;
                    }
                } else if let Ok(mut g) = inner_d.lock() {
                    g.error = Some(std::sync::Arc::new(crate::error::Error::JS(
                        "No QR found in image".into(),
                    )));
                }
                ctx_d.request_repaint();
            }
        });
    }
    #[cfg(target_os = "android")]
    {
        let _ = (inner, ctx);
    }
}

#[cfg(not(target_os = "android"))]
fn decode_bytes(data: &[u8]) -> Option<String> {
    #[cfg(all(feature = "camera", feature = "qr"))]
    {
        decode_with_image(data)
    }
    #[cfg(not(all(feature = "camera", feature = "qr")))]
    {
        let _ = data;
        None
    }
}

#[cfg(all(feature = "camera", feature = "qr", not(target_os = "android")))]
fn decode_with_image(data: &[u8]) -> Option<String> {
    let img = image::load_from_memory(data).ok()?;
    let rgba = img.to_rgba8();
    let (w, h) = (rgba.width(), rgba.height());
    let raw = rgba.into_raw();
    crate::qr::decode_qr_rgba(&raw, w, h)
}

#[cfg(all(target_arch = "wasm32", feature = "web"))]
fn map_camera_error(e: &crate::error::Error) -> crate::error::Error {
    match e {
        crate::error::Error::JS(msg) => {
            if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
                crate::error::Error::CameraPermissionDenied(msg.clone())
            } else {
                crate::error::Error::CameraNotAvailable(msg.clone())
            }
        }
        other => crate::error::Error::CameraNotAvailable(other.to_string()),
    }
}
