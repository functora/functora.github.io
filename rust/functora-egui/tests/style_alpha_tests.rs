//! Style alpha regression: re-alphaing a color must preserve its hue.
//! `Color32` stores premultiplied channels, so naive `.r()/.g()/.b()`
//! round-trips dim the hue a second time.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::ResponsiveExt;

fn hue_close(first: [u8; 4], second: [u8; 4]) -> bool {
    first[3] == second[3]
        && first[..3]
            .iter()
            .zip(second[..3].iter())
            .all(|(got, want)| (i16::from(*got) - i16::from(*want)).abs() <= 1)
}

fn spacing() -> functora_egui::responsive::Spacing {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        ..Default::default()
    };
    let mut result = None;
    let mut output = ctx.run_ui(raw, |ui| {
        result = Some(ui.responsive_spacing());
    });
    output.textures_delta.clear();
    result.unwrap_or_else(|| {
        panic!("responsive spacing must resolve");
    })
}

#[test]
fn button_disabled_bg_preserves_hue() {
    let probe = egui::Color32::from_rgba_unmultiplied(200, 100, 50, 200);
    let mut theme = functora_egui::theme::shadcn_theme_dark::dark();
    theme.primary = probe;
    let spacing = spacing();
    let style = functora_egui::widgets::button::button_variant_style::resolve_button_style(
        &theme,
        &spacing,
        functora_egui::ButtonVariant::Default,
        functora_egui::ComponentSize::Default,
        false,
        false,
        true,
    );
    assert!(
        hue_close(style.bg.to_srgba_unmultiplied(), [200, 100, 50, 128]),
        "disabled bg must keep the probe hue, got {:?}",
        style.bg.to_srgba_unmultiplied()
    );
}

#[test]
fn button_opaque_bg_realpha_is_stable() {
    let theme = functora_egui::theme::shadcn_theme_dark::dark();
    let spacing = spacing();
    let style = functora_egui::widgets::button::button_variant_style::resolve_button_style(
        &theme,
        &spacing,
        functora_egui::ButtonVariant::Default,
        functora_egui::ComponentSize::Default,
        false,
        false,
        true,
    );
    assert!(
        hue_close(
            style.bg.to_srgba_unmultiplied(),
            [theme.primary.r(), theme.primary.g(), theme.primary.b(), 128]
        ),
        "opaque bg must survive re-alpha, got {:?}",
        style.bg.to_srgba_unmultiplied()
    );
}

#[test]
fn switch_disabled_track_preserves_hue() {
    let probe = egui::Color32::from_rgba_unmultiplied(200, 100, 50, 200);
    let mut theme = functora_egui::theme::shadcn_theme_dark::dark();
    theme.muted = probe;
    let style = functora_egui::widgets::switch::switch_style::resolve_switch_style(
        &theme, false, 0.0, true,
    );
    assert!(
        hue_close(
            style.track_color.to_srgba_unmultiplied(),
            [200, 100, 50, 110]
        ),
        "disabled track must keep the probe hue, got {:?}",
        style.track_color.to_srgba_unmultiplied()
    );
}

#[test]
fn switch_opaque_track_realpha_is_stable() {
    let theme = functora_egui::theme::shadcn_theme_dark::dark();
    let style = functora_egui::widgets::switch::switch_style::resolve_switch_style(
        &theme, false, 0.0, true,
    );
    assert_eq!(style.track_color.a(), 110);
    assert!(
        hue_close(
            style.track_color.to_srgba_unmultiplied(),
            [theme.muted.r(), theme.muted.g(), theme.muted.b(), 110]
        ),
        "opaque muted must survive re-alpha, got {:?}",
        style.track_color.to_srgba_unmultiplied()
    );
}

#[test]
fn interpolate_endpoints_return_inputs() {
    let first = egui::Color32::from_rgba_unmultiplied(200, 100, 50, 110);
    let second = egui::Color32::from_rgba_unmultiplied(10, 200, 90, 110);
    let at_zero = functora_egui::paint::interpolate_color::interpolate_color(first, second, 0.0);
    let at_one = functora_egui::paint::interpolate_color::interpolate_color(first, second, 1.0);
    assert!(
        hue_close(at_zero.to_srgba_unmultiplied(), [200, 100, 50, 110]),
        "t=0 must return the first color, got {:?}",
        at_zero.to_srgba_unmultiplied()
    );
    assert!(
        hue_close(at_one.to_srgba_unmultiplied(), [10, 200, 90, 110]),
        "t=1 must return the second color, got {:?}",
        at_one.to_srgba_unmultiplied()
    );
}
