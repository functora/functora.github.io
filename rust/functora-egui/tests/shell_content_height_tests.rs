//! Shell content height: the shell column must hand page content a usable
//! available height, not the ~interact-size strip that `Ui::horizontal`
//! alone would allocate. Widgets that size from available space (e.g.
//! `egui::Image` with the default `Fraction(1,1)` fit) collapse to zero
//! when the height is exhausted by preceding labels.

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

fn shell_content_available(screen: egui::Vec2) -> egui::Vec2 {
    let ctx = egui::Context::default();
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(egui::Pos2::ZERO, screen)),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let available = std::cell::Cell::new(egui::Vec2::ZERO);
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
            let _ = content.label("name.jpg");
            let _ = content.label("1.0 MB");
            content.add_space(8.0);
            available.set(content.available_size());
        });
    });
    out.textures_delta.clear();
    available.get()
}

#[test]
fn shell_content_available_height_is_usable_after_leading_widgets() {
    let available = shell_content_available(egui::vec2(1280.0, 800.0));
    assert!(
        available.y > 100.0,
        "shell content must leave usable height after leading labels on desktop, got {available:?}"
    );
}

#[test]
fn shell_content_available_height_is_usable_on_mobile() {
    let available = shell_content_available(egui::vec2(390.0, 844.0));
    assert!(
        available.y > 100.0,
        "shell content must leave usable height after leading labels on mobile, got {available:?}"
    );
}
