use crate::responsive::breakpoint::Breakpoint;

#[cfg(target_arch = "wasm32")]
#[must_use]
pub fn startup_width(cc: &eframe::CreationContext<'_>) -> f32 {
    web_sys::window()
        .and_then(|win| {
            win.visual_viewport()
                .map(|vp| vp.width() as f32)
                .or_else(|| {
                    win.inner_width()
                        .ok()
                        .and_then(|v| v.as_f64())
                        .map(|v| v as f32)
                })
        })
        .unwrap_or_else(|| cc.egui_ctx.input(|input| input.viewport_rect().width()))
}

#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn startup_width(ctx: &egui::Context) -> f32 {
    ctx.input(|input| input.viewport_rect().width())
}

#[cfg(target_arch = "wasm32")]
#[must_use]
pub fn is_mobile_initial(cc: &eframe::CreationContext<'_>) -> bool {
    let width = startup_width(cc);
    if width == 0.0 {
        true
    } else {
        width < Breakpoint::MOBILE_MAX_WIDTH
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn is_mobile_initial_ctx(ctx: &egui::Context) -> bool {
    let width = startup_width(ctx);
    if width == 0.0 {
        true
    } else {
        width < Breakpoint::MOBILE_MAX_WIDTH
    }
}

#[must_use]
pub fn is_mobile(ctx: &egui::Context) -> bool {
    crate::responsive::responsive_ext::ResponsiveExt::on_mobile(ctx)
}
