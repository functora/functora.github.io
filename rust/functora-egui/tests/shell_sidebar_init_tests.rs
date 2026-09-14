//! Shell sidebar init: the shell owns the responsive collapsed default,
//! so every app starts collapsed on mobile and expanded on desktop.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Route {
    #[default]
    Home,
}

impl std::fmt::Display for Route {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "home")
    }
}

impl std::str::FromStr for Route {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            _ => Err("unknown".into()),
        }
    }
}

impl functora_egui::route::RouteMetadata for Route {
    fn label(&self, _lang: functora_core::i18n::Language) -> Cow<'static, str> {
        "Home".into()
    }

    fn parent(&self) -> Option<Self> {
        None
    }

    fn children(&self) -> Vec<Self> {
        vec![]
    }

    fn kind(&self) -> functora_egui::route::RouteKind {
        functora_egui::route::RouteKind::Page
    }
}

fn run_shell(ctx: &egui::Context, width: f32, collapsed: &mut bool) {
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::Vec2::new(width, 800.0),
        )),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let route = Route::Home;
        let history = functora_egui::NavHistory::new(Route::Home);
        let _ = functora_egui::Shell::new("App", collapsed, |side| {
            let _ = functora_egui::Button::new("Home").full_width().show(side);
            false
        })
        .breadcrumb(&route, &history)
        .show(ui, |_| {});
    });
    out.textures_delta.clear();
}

#[test]
fn shell_collapses_wrong_initial_on_mobile() {
    let ctx = egui::Context::default();
    let mut collapsed = false;
    run_shell(&ctx, 375.0, &mut collapsed);
    assert!(
        collapsed,
        "shell must collapse an expanded sidebar on first mobile frame, got expanded"
    );
}

#[test]
fn shell_expands_wrong_initial_on_desktop() {
    let ctx = egui::Context::default();
    let mut collapsed = true;
    run_shell(&ctx, 1280.0, &mut collapsed);
    assert!(
        !collapsed,
        "shell must expand a collapsed sidebar on first desktop frame, got collapsed"
    );
}

#[test]
fn shell_defaults_to_collapsed_on_unknown_width() {
    let ctx = egui::Context::default();
    let mut collapsed = false;
    run_shell(&ctx, 0.0, &mut collapsed);
    assert!(
        collapsed,
        "shell must default to collapsed while viewport width is unknown"
    );
}

#[test]
fn shell_collapses_on_desktop_to_mobile_transition() {
    let ctx = egui::Context::default();
    let mut collapsed = false;
    run_shell(&ctx, 1280.0, &mut collapsed);
    assert!(!collapsed);
    run_shell(&ctx, 375.0, &mut collapsed);
    assert!(
        collapsed,
        "shell must collapse when crossing from desktop to mobile"
    );
}

#[test]
fn shell_expands_on_mobile_to_desktop_transition() {
    let ctx = egui::Context::default();
    let mut collapsed = true;
    run_shell(&ctx, 375.0, &mut collapsed);
    assert!(collapsed);
    run_shell(&ctx, 1280.0, &mut collapsed);
    assert!(
        !collapsed,
        "shell must expand when crossing from mobile to desktop"
    );
}

#[test]
fn initial_sidebar_collapsed_matches_breakpoint() {
    let ctx = egui::Context::default();
    let raw_mobile = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::Vec2::new(375.0, 800.0),
        )),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw_mobile, |ui| {
        assert!(functora_egui::initial_sidebar_collapsed(ui.ctx()));
    });
    out.textures_delta.clear();
    let ctx_desktop = egui::Context::default();
    let raw_desktop = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::Vec2::new(1280.0, 800.0),
        )),
        ..Default::default()
    };
    let mut out_desktop = ctx_desktop.run_ui(raw_desktop, |ui| {
        assert!(!functora_egui::initial_sidebar_collapsed(ui.ctx()));
    });
    out_desktop.textures_delta.clear();
}
