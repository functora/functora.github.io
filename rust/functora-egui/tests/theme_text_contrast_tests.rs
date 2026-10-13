use functora_egui::theme::shadcn_theme::ShadcnTheme;
use functora_egui::theme::shadcn_theme_ext::ShadcnThemeExt;
use functora_egui::theme_extra::Theme;

fn expected_palette(theme: Theme) -> ShadcnTheme {
    match theme {
        Theme::Light => functora_egui::theme::shadcn_theme_light::light(),
        Theme::Dark => functora_egui::theme::shadcn_theme_dark::dark(),
    }
}

fn expected_egui_theme(theme: Theme) -> egui::Theme {
    match theme {
        Theme::Light => egui::Theme::Light,
        Theme::Dark => egui::Theme::Dark,
    }
}

#[test]
fn text_tokens_follow_selected_theme() {
    for theme in [Theme::Light, Theme::Dark] {
        let ctx = egui::Context::default();
        functora_egui::theme_extra::set_theme(&ctx, theme);
        let palette = expected_palette(theme);
        assert_eq!(
            ctx.theme(),
            expected_egui_theme(theme),
            "egui style selection must follow the functora theme for {theme:?}"
        );
        let style = ctx.style_of(expected_egui_theme(theme));
        let visuals = &style.visuals;
        assert_eq!(
            visuals.override_text_color,
            Some(palette.foreground),
            "plain labels must render in foreground for {theme:?}"
        );
        assert_eq!(
            visuals.weak_text_color,
            Some(palette.muted_foreground),
            "weak text must render in muted foreground for {theme:?}"
        );
        assert_eq!(
            visuals.hyperlink_color, palette.primary,
            "links must render in primary for {theme:?}"
        );
        for widgets in [
            &visuals.widgets.noninteractive,
            &visuals.widgets.inactive,
            &visuals.widgets.active,
        ] {
            assert_eq!(
                widgets.fg_stroke.color, palette.foreground,
                "widget text must render in foreground for {theme:?}"
            );
        }
        assert_eq!(
            visuals.selection.bg_fill, palette.primary,
            "selection must use primary for {theme:?}"
        );
        assert_eq!(
            visuals.selection.stroke,
            egui::Stroke::new(1.0, palette.primary_foreground),
            "selection stroke must use primary foreground for {theme:?}"
        );
        assert_eq!(
            visuals.extreme_bg_color, palette.secondary,
            "extreme background must use secondary for {theme:?}"
        );
        assert_eq!(
            visuals.code_bg_color, palette.accent,
            "code background must use accent for {theme:?}"
        );
        assert_eq!(
            visuals.faint_bg_color, palette.secondary,
            "faint background must use secondary for {theme:?}"
        );
        assert_eq!(
            ctx.shadcn_theme().background,
            palette.background,
            "stored shadcn theme must match the selected palette for {theme:?}"
        );
    }
}
