use crate::theme::shadcn_theme_ext::ShadcnThemeExt;

#[cfg(feature = "qr")]
const TEX_SIDE: u32 = 256;
const FALLBACK_SIDE: f32 = 320.0;

impl egui::Widget for super::widget::QrImage<'_> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = ui.ctx().shadcn_theme();
        let outer = ui.available_width();
        egui::Frame::new()
            .fill(egui::Color32::WHITE)
            .stroke(egui::Stroke::new(1.0, theme.border))
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                theme.radius,
            )))
            .inner_margin(egui::Margin::same(12))
            .show(ui, |inner| {
                let side = [inner.available_width(), outer, FALLBACK_SIDE]
                    .into_iter()
                    .find(|side| side.is_finite() && *side > 0.0)
                    .unwrap_or(FALLBACK_SIDE);
                match cached_texture(inner.ctx(), self.content) {
                    Some(tex) => inner.add(egui::Image::new((tex.id(), egui::Vec2::splat(side)))),
                    None => fallback(inner, side),
                }
            })
            .response
    }
}

fn cached_texture(ctx: &egui::Context, content: &str) -> Option<egui::TextureHandle> {
    if content.is_empty() {
        None
    } else {
        let hash = egui::util::hash(content);
        let key = egui::Id::new(("functora-qr", hash));
        ctx.data(|data| data.get_temp::<egui::TextureHandle>(key))
            .or_else(|| {
                qr_color_image(content).map(|image| {
                    let tex = ctx.load_texture(
                        format!("functora-qr-{hash:016x}"),
                        image,
                        egui::TextureOptions::NEAREST,
                    );
                    let _ = ctx.data_mut(|data| data.insert_temp(key, tex.clone()));
                    tex
                })
            })
    }
}

#[cfg(feature = "qr")]
fn qr_color_image(content: &str) -> Option<egui::ColorImage> {
    crate::qr::qr_rgba(content, TEX_SIDE).and_then(|(width, height, rgba)| {
        usize::try_from(width)
            .ok()
            .zip(usize::try_from(height).ok())
            .and_then(|(wide, high)| {
                wide.checked_mul(high).and_then(|pixels| {
                    pixels.checked_mul(4).and_then(|expected| {
                        (expected == rgba.len())
                            .then(|| egui::ColorImage::from_rgba_unmultiplied([wide, high], &rgba))
                    })
                })
            })
    })
}

#[cfg(not(feature = "qr"))]
fn qr_color_image(_content: &str) -> Option<egui::ColorImage> {
    None
}

fn fallback(ui: &mut egui::Ui, side: f32) -> egui::Response {
    let theme = ui.ctx().shadcn_theme();
    let (rect, response) = ui.allocate_exact_size(egui::Vec2::splat(side), egui::Sense::hover());
    if ui.is_rect_visible(rect) {
        let _ = ui.painter().rect_filled(
            rect,
            egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius)),
            theme.muted,
        );
        let _ = ui.painter().text(
            rect.center(),
            egui::Align2::CENTER_CENTER,
            "QR unavailable",
            egui::FontId::proportional(13.0),
            theme.muted_foreground,
        );
    }
    response
}
