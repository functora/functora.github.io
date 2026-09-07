//! `AreaChart` demo regression: chart lines must contrast with the card surface
//! on both themes, and the legend pills must not share one fill color.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::theme_extra::{Theme, set_theme};

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step_themed(
        &mut self,
        theme: Theme,
        height: f32,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        set_theme(&self.ctx, theme);
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, height))),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}
fn max_channel_diff(hue: [u8; 4], bg: egui::Color32) -> u8 {
    hue[0]
        .abs_diff(bg.r())
        .max(hue[1].abs_diff(bg.g()))
        .max(hue[2].abs_diff(bg.b()))
}

fn chart_line_hues(out: &egui::FullOutput) -> Vec<[u8; 4]> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Path(path) if path.stroke.width > 1.0 => match path.stroke.color {
                egui::epaint::ColorMode::Solid(color) => Some(color.to_srgba_unmultiplied()),
                egui::epaint::ColorMode::UV(_) => None,
            },
            _ => None,
        })
        .collect()
}

fn primary_fills(out: &egui::FullOutput, primary: egui::Color32) -> usize {
    out.shapes
        .iter()
        .filter(|clipped| matches!(&clipped.shape, egui::Shape::Rect(rect) if rect.fill == primary))
        .count()
}

fn render_demo(ui: &mut egui::Ui) {
    functora_egui_demo::ShowcaseApp::demo_area_chart(ui);
}

#[test]
fn chart_lines_contrast_with_card() {
    for theme in [Theme::Light, Theme::Dark] {
        let mut app = Harness::new();
        let mut body = render_demo;
        let out = app.step_themed(theme, 800.0, &mut body);
        let card = functora_egui::ShadcnThemeExt::shadcn_theme(&app.ctx).card;
        let lines = chart_line_hues(&out);
        assert_eq!(lines.len(), 2, "demo renders two series lines on {theme}");
        for hue in lines {
            assert!(
                max_channel_diff(hue, card) >= 50,
                "chart line {hue:?} must contrast with card {card:?} on {theme}"
            );
        }
    }
}

#[test]
fn legend_pills_have_distinct_fills() {
    let mut app = Harness::new();
    let mut body = render_demo;
    let out = app.step_themed(Theme::Dark, 800.0, &mut body);
    let theme = functora_egui::ShadcnThemeExt::shadcn_theme(&app.ctx);
    assert_eq!(
        primary_fills(&out, theme.primary),
        1,
        "legend must not paint two identical Default pills"
    );
}
