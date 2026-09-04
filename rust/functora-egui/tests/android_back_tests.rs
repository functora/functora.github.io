#![allow(clippy::unwrap_used, clippy::expect_used)]
use egui::{Context, Event, Modifiers, Pos2, RawInput, Rect, Vec2};
use functora_egui::{BackOutcome, NavHistory, Shell, is_back_pressed};
use std::fmt::Display;
use std::str::FromStr;

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum DemoRoute {
    #[default]
    Home,
    PageA,
    PageB,
}

impl Display for DemoRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Home => write!(f, "home"),
            Self::PageA => write!(f, "page_a"),
            Self::PageB => write!(f, "page_b"),
        }
    }
}

impl FromStr for DemoRoute {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            "page_a" => Ok(Self::PageA),
            "page_b" => Ok(Self::PageB),
            _ => Err(format!("unknown {s}")),
        }
    }
}

impl functora_egui::route::RouteMetadata for DemoRoute {
    fn label(&self, _lang: functora_core::i18n::Language) -> std::borrow::Cow<'static, str> {
        match self {
            Self::Home => "Home".into(),
            Self::PageA => "Page A".into(),
            Self::PageB => "Page B".into(),
        }
    }
    fn parent(&self) -> Option<Self> {
        match self {
            Self::PageA | Self::PageB => Some(Self::Home),
            Self::Home => None,
        }
    }
    fn children(&self) -> Vec<Self> {
        match self {
            Self::Home => vec![Self::PageA, Self::PageB],
            _ => vec![],
        }
    }
}

fn ctx_with_events(events: Vec<Event>) -> (Context, egui::RawInput) {
    let ctx = Context::default();
    ctx.memory_mut(|m| {
        m.options.max_passes = 1.try_into().unwrap();
    });
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        time: Some(0.0),
        events,
        ..Default::default()
    };
    (ctx, raw)
}

#[test]
fn is_back_pressed_false_without_key() {
    let (ctx, raw) = ctx_with_events(vec![]);
    let mut out = ctx.run_ui(raw, |ui| {
        assert!(!is_back_pressed(ui.ctx()));
    });
    out.textures_delta.clear();
}

#[test]
fn is_back_pressed_true_with_browser_back() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        assert!(is_back_pressed(ui.ctx()));
    });
    out.textures_delta.clear();
}

#[test]
fn handle_system_back_navigates_when_can() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let egui_ctx = ui.ctx();
        let mut called = false;
        let outcome = functora_egui::handle_system_back(egui_ctx, true, || called = true);
        assert_eq!(outcome, Some(BackOutcome::ConsumedNav));
        assert!(called);
    });
    out.textures_delta.clear();
}

#[test]
fn handle_system_back_noop_at_root() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let egui_ctx = ui.ctx();
        let mut called = false;
        let outcome = functora_egui::handle_system_back(egui_ctx, false, || called = true);
        assert_eq!(outcome, Some(BackOutcome::ConsumedNoop));
        assert!(!called);
    });
    out.textures_delta.clear();
}

#[test]
fn handle_system_back_none_without_press() {
    let (ctx, raw) = ctx_with_events(vec![]);
    let mut out = ctx.run_ui(raw, |ui| {
        let outcome = functora_egui::handle_system_back(ui.ctx(), true, || {});
        assert_eq!(outcome, None);
    });
    out.textures_delta.clear();
}

#[test]
fn app_router_handle_back_goes_back() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let egui_ctx = ui.ctx();
        let mut state = ();
        let mut router =
            functora_egui::route::AppRouter::<DemoRoute, ()>::new(&mut state, DemoRoute::Home);
        router.navigate(&mut state, DemoRoute::PageA);
        router.navigate(&mut state, DemoRoute::PageB);
        assert_eq!(router.current(), &DemoRoute::PageB);
        let outcome = router.handle_back(egui_ctx, &mut state);
        assert_eq!(outcome, Some(BackOutcome::ConsumedNav));
        assert_eq!(router.current(), &DemoRoute::PageA);
        assert!(router.can_go_back());
    });
    out.textures_delta.clear();
}

#[test]
fn app_router_handle_back_noop_at_root() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let egui_ctx = ui.ctx();
        let mut state = ();
        let mut router =
            functora_egui::route::AppRouter::<DemoRoute, ()>::new(&mut state, DemoRoute::Home);
        assert_eq!(router.current(), &DemoRoute::Home);
        let outcome = router.handle_back(egui_ctx, &mut state);
        assert_eq!(outcome, Some(BackOutcome::ConsumedNoop));
        assert_eq!(router.current(), &DemoRoute::Home);
    });
    out.textures_delta.clear();
}

#[test]
fn app_router_system_back_opt_out() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let egui_ctx = ui.ctx();
        let mut state = ();
        let mut router =
            functora_egui::route::AppRouter::<DemoRoute, ()>::new(&mut state, DemoRoute::Home)
                .with_system_back(false);
        router.navigate(&mut state, DemoRoute::PageA);
        let outcome = router.handle_back(egui_ctx, &mut state);
        assert_eq!(outcome, None);
        assert_eq!(router.current(), &DemoRoute::PageA);
    });
    out.textures_delta.clear();
}

#[test]
fn shell_system_back_synthesizes_nav_action() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let history = {
            let mut h = NavHistory::new(DemoRoute::Home);
            h.push(DemoRoute::PageA);
            h
        };
        let route = DemoRoute::PageA;
        let mut collapsed = true;
        let action = Shell::new("App", &mut collapsed, |_: &mut egui::Ui| false)
            .breadcrumb(&route, &history)
            .show(ui, |_| {});
        assert!(matches!(action, Some(functora_egui::NavAction::Back)));
    });
    out.textures_delta.clear();
}

#[test]
fn shell_system_back_noop_at_root_returns_none() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let history = NavHistory::new(DemoRoute::Home);
        let route = DemoRoute::Home;
        let mut collapsed = true;
        let action = Shell::new("App", &mut collapsed, |_: &mut egui::Ui| false)
            .breadcrumb(&route, &history)
            .show(ui, |_| {});
        assert!(action.is_none());
    });
    out.textures_delta.clear();
}

#[test]
fn shell_system_back_opt_out_no_action() {
    let (ctx, raw) = ctx_with_events(vec![Event::Key {
        key: egui::Key::BrowserBack,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers::default(),
    }]);
    let mut out = ctx.run_ui(raw, |ui| {
        let history = {
            let mut h = NavHistory::new(DemoRoute::Home);
            h.push(DemoRoute::PageA);
            h
        };
        let route = DemoRoute::PageA;
        let mut collapsed = true;
        let action = Shell::new("App", &mut collapsed, |_: &mut egui::Ui| false)
            .breadcrumb(&route, &history)
            .system_back(false)
            .show(ui, |_| {});
        assert!(action.is_none());
    });
    out.textures_delta.clear();
}

#[test]
fn nav_history_stays_consistent_after_back_press() {
    let mut h = NavHistory::new(DemoRoute::Home);
    h.push(DemoRoute::PageA);
    h.push(DemoRoute::PageB);
    assert_eq!(h.current(), &DemoRoute::PageB);
    let _ = h.go_back();
    assert_eq!(h.current(), &DemoRoute::PageA);
    let _ = h.go_back();
    assert_eq!(h.current(), &DemoRoute::Home);
    assert!(!h.can_go_back());
}
