//! Shell content width: text screens must lay their paragraphs out near
//! the full content width of the shell column, on any viewport width.

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

const LICENSE_TEXT: &str = "Copyright (c) 2026 Functora. Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files. Additional filler text to make the paragraph long enough to wrap across the full content width of the shell column on a wide desktop viewport.";

fn max_text_width(out: &egui::FullOutput) -> f32 {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.size().x),
            _ => None,
        })
        .fold(0.0, f32::max)
}

/// Mimics a text screen like License/Privacy: title, scroll area with long
/// text, trailing button — rendered inside the real `Shell`.
fn render_license_like() -> egui::FullOutput {
    let ctx = egui::Context::default();
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::Vec2::new(1280.0, 800.0),
        )),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let mut collapsed = true;
        let route = Route::Home;
        let history = functora_egui::NavHistory::new(Route::Home);
        let _ = functora_egui::Shell::new("App", &mut collapsed, |side| {
            let _ = functora_egui::Button::new("Home").full_width().show(side);
            false
        })
        .breadcrumb(&route, &history)
        .show(ui, |content| {
            let _ = content.label(egui::RichText::new("License").size(20.0).strong());
            content.add_space(8.0);
            let _ = egui::ScrollArea::vertical().show(content, |inner| {
                let _ = inner.label(LICENSE_TEXT);
            });
            content.add_space(8.0);
            let _ = functora_egui::Button::new("Back").show(content);
        });
    });
    out.textures_delta.clear();
    out
}

#[test]
fn shell_content_column_fills_available_width() {
    let out = render_license_like();
    let widest = max_text_width(&out);
    assert!(
        widest > 1000.0,
        "license-like text must wrap near full content width (~1160px on 1280 viewport), got {widest:.0}px — column is shrink-wrapped"
    );
}
