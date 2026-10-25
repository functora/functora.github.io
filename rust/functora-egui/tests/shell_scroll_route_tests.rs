//! Shell content scroll: jumping to another screen resets the scrollable
//! area to the top instead of inheriting the previous screen's offset.

use std::borrow::Cow;
use std::cell::Cell;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Route {
    #[default]
    Home,
    Away,
}

impl std::fmt::Display for Route {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Home => write!(f, "home"),
            Self::Away => write!(f, "away"),
        }
    }
}

impl std::str::FromStr for Route {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            "away" => Ok(Self::Away),
            _ => Err("unknown".into()),
        }
    }
}

impl functora_egui::route::RouteMetadata for Route {
    fn label(&self, _lang: functora_core::i18n::Language) -> Cow<'static, str> {
        match self {
            Self::Home => "Home".into(),
            Self::Away => "Away".into(),
        }
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

fn scroll_down_events() -> Vec<egui::Event> {
    std::iter::repeat_n(
        egui::Event::MouseWheel {
            unit: egui::MouseWheelUnit::Point,
            delta: egui::vec2(0.0, -5.0),
            phase: egui::TouchPhase::Move,
            modifiers: egui::Modifiers::NONE,
        },
        100,
    )
    .collect()
}

fn run_frame(ctx: &egui::Context, route: Route, input: &[egui::Event], frame: u32) -> f32 {
    let mut events = vec![egui::Event::PointerMoved(egui::pos2(200.0, 200.0))];
    events.extend_from_slice(input);
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::vec2(600.0, 400.0),
        )),
        time: Some(f64::from(frame) / 60.0),
        events,
        ..Default::default()
    };
    let spacer_y = Cell::new(f32::NAN);
    let mut out = ctx.run_ui(raw, |ui| {
        let mut collapsed = true;
        let history = functora_egui::NavHistory::new(route);
        let _ = functora_egui::Shell::new("App", &mut collapsed, |side| {
            let _ = functora_egui::Button::new("Nav").full_width().show(side);
            false
        })
        .breadcrumb(&route, &history)
        .show(ui, |content| {
            let (rect, _) =
                content.allocate_exact_size(egui::vec2(50.0, 3000.0), egui::Sense::hover());
            spacer_y.set(rect.min.y);
        });
    });
    out.textures_delta.clear();
    spacer_y.get()
}

#[test]
fn shell_content_scroll_resets_on_route_change() {
    let ctx = egui::Context::default();
    let y_top = run_frame(&ctx, Route::Home, &[], 1);
    let scroll = scroll_down_events();
    let y_scrolled = run_frame(&ctx, Route::Home, &scroll, 2);
    assert!(
        (y_scrolled - y_top).abs() > 1.0,
        "scroll event must move the content (y_top={y_top}, y_scrolled={y_scrolled})"
    );
    let y_after = run_frame(&ctx, Route::Away, &[], 3);
    assert!(
        (y_after - y_top).abs() < 0.5,
        "route change must reset content scroll to top (y_top={y_top}, y_after={y_after})"
    );
}
