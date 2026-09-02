use crate::progress::{Job, Stage};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

pub struct BlockingOverlay {
    title: String,
    description: Option<String>,
}

impl BlockingOverlay {
    #[must_use]
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            description: None,
        }
    }

    #[must_use]
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    pub fn show(
        self,
        ctx: &egui::Context,
        open: &mut bool,
        job: Option<&Job<Stage>>,
        cancel: &Arc<AtomicBool>,
    ) {
        if !*open {
            return;
        }
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);
        let screen = ctx.input(egui::InputState::viewport_rect);
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("blocking_backdrop"));
        let painter = ctx.layer_painter(backdrop_layer);
        let _ = painter.rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(80),
        );
        let _ = egui::Area::new(egui::Id::new("blocking_backdrop_sense"))
            .order(egui::Order::Middle)
            .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
            .show(ctx, |ui| {
                let (_, response) = ui.allocate_exact_size(screen.size(), egui::Sense::click());
                response
            });
        let max_panel_width = (screen.width() - 2.0 * spacing.page_padding - 50.0).max(0.0);
        let panel_width = 380.0_f32.clamp(0.0, max_panel_width);
        let _ = egui::Area::new(egui::Id::new("blocking_panel"))
            .order(egui::Order::Foreground)
            .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
            .show(ctx, |ui| {
                let frame = egui::Frame::NONE
                    .fill(theme.card)
                    .inner_margin(egui::Margin::same(24))
                    .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                        theme.radius + 2.0,
                    )))
                    .stroke(egui::Stroke::new(1.0, theme.border))
                    .shadow(egui::Shadow {
                        offset: [0, 8],
                        blur: 24,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(12),
                    });
                let _ = frame.show(ui, |content_ui| {
                    content_ui.set_max_width(panel_width);
                    let _ = content_ui.vertical(|inner| {
                        let _ = inner.horizontal(|row| {
                            let _ = row.add(crate::Spinner::new().size(18.0));
                            row.add_space(8.0);
                            let _ = row.label(
                                egui::RichText::new(&self.title)
                                    .color(theme.foreground)
                                    .size(16.0)
                                    .strong(),
                            );
                        });
                        if let Some(desc) = &self.description {
                            inner.add_space(8.0);
                            let _ = inner.label(
                                egui::RichText::new(desc)
                                    .color(theme.muted_foreground)
                                    .size(13.0),
                            );
                        }
                        if let Some(j) = job {
                            inner.add_space(12.0);
                            let pct = f32::from(j.percent()) / 100.0;
                            let _ = inner.add(crate::Progress::new(pct));
                            inner.add_space(6.0);
                            let name = j.name.as_deref().unwrap_or("");
                            let label = if name.is_empty() {
                                format!(
                                    "{} / {} ({}%)",
                                    crate::files::format_size(j.done),
                                    crate::files::format_size(j.total),
                                    j.percent()
                                )
                            } else {
                                format!(
                                    "{name}: {} / {} ({}%)",
                                    crate::files::format_size(j.done),
                                    crate::files::format_size(j.total),
                                    j.percent()
                                )
                            };
                            let _ = inner.label(
                                egui::RichText::new(label)
                                    .color(theme.muted_foreground)
                                    .size(12.0),
                            );
                        } else {
                            inner.add_space(12.0);
                            let _ = inner.label(
                                egui::RichText::new("Preparing...")
                                    .color(theme.muted_foreground)
                                    .size(12.0),
                            );
                        }
                        inner.add_space(16.0);
                        let is_cancelling = cancel.load(Ordering::Relaxed);
                        let _ = inner.with_layout(
                            egui::Layout::right_to_left(egui::Align::Center),
                            |right| {
                                if is_cancelling {
                                    let _ = right.add(
                                        crate::Button::new("Cancelling...")
                                            .variant(crate::ButtonVariant::Outline)
                                            .size(crate::ComponentSize::Sm)
                                            .enabled(false),
                                    );
                                } else if right
                                    .add(
                                        crate::Button::new("Cancel")
                                            .variant(crate::ButtonVariant::Outline)
                                            .size(crate::ComponentSize::Sm),
                                    )
                                    .clicked()
                                {
                                    cancel.store(true, Ordering::Relaxed);
                                }
                            },
                        );
                    });
                });
            });
        ctx.request_repaint();
    }
}
