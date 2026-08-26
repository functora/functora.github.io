use super::camera_view_state::CameraViewState;
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;

/// A live camera feed widget: `aspect-video` preview driven by the shared
/// platform frame pump.
#[must_use]
pub struct CameraView {
    desired_size: egui::Vec2,
    fps: f32,
    auto_start: bool,
    controls: bool,
}

impl Default for CameraView {
    fn default() -> Self {
        Self {
            desired_size: egui::vec2(320.0, 240.0),
            fps: 15.0,
            auto_start: true,
            controls: false,
        }
    }
}

impl CameraView {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn desired_size(mut self, size: egui::Vec2) -> Self {
        self.desired_size = size;
        self
    }

    pub fn fps(mut self, fps: f32) -> Self {
        self.fps = fps;
        self
    }

    pub fn auto_start(mut self, yes: bool) -> Self {
        self.auto_start = yes;
        self
    }

    pub fn controls(mut self, yes: bool) -> Self {
        self.controls = yes;
        self
    }

    /// Renders the feed. The state must have a handler installed (see
    /// `CameraViewState::set_handler`); without one only errors are shown.
    pub fn show(self, ui: &mut egui::Ui, state: &mut CameraViewState) -> egui::Response {
        let theme = ui.ctx().shadcn_theme();
        if self.auto_start && !state.is_running() && state.error().is_none() {
            state.set_fps(self.fps);
            let _ = state.start(ui.ctx());
        }
        let frame = egui::Frame::new()
            .fill(theme.card)
            .stroke(egui::Stroke::new(1.0, theme.border))
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                theme.radius,
            )))
            .inner_margin(egui::Margin::same(12))
            .show(ui, |inner| {
                if let Some((rgba, w, h)) = state.drain_rgba() {
                    state.store_texture(inner.ctx(), &rgba, w, h);
                }
                let running = state.is_running();
                let error = state.error();
                match state.take_texture().clone() {
                    Some(tex) => {
                        let _ = inner.add(
                            egui::Image::new((tex.id(), self.desired_size))
                                .corner_radius(egui::CornerRadius::same(8)),
                        );
                        if running {
                            inner.ctx().request_repaint();
                        }
                    }
                    None if running => {
                        placeholder(inner, self.desired_size, &theme, "Starting camera…");
                        inner.ctx().request_repaint();
                    }
                    None => {
                        placeholder(inner, self.desired_size, &theme, "Camera off");
                    }
                }
                if let Some(err) = error {
                    inner.add_space(6.0);
                    let _ = inner.label(
                        egui::RichText::new(err.to_string())
                            .color(theme.destructive)
                            .size(12.0),
                    );
                }
                if self.controls {
                    inner.add_space(8.0);
                    let _ = inner.horizontal(|row| {
                        let label = if running { "Stop" } else { "Start" };
                        if row
                            .add(crate::Button::new(label).variant(crate::ButtonVariant::Secondary))
                            .clicked()
                        {
                            if running {
                                state.stop();
                            } else {
                                state.set_fps(self.fps);
                                let _ = state.start(row.ctx());
                            }
                        }
                    });
                }
            });
        frame.response
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
