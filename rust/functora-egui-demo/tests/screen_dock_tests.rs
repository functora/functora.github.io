//! The screen dock at the bottom of every screen: it jumps to the previous
//! and the next screen in the catalog order the sidebar, the command palette
//! and the overview share.

use egui::{Context, Event, Pos2, RawInput, Rect, Vec2};
use functora_egui::i18n::Language;
use functora_egui_demo::route::AppRoute;
use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

/// The screens in the order the sidebar and the command palette list them.
fn catalog_routes() -> Vec<AppRoute> {
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .map(|def| def.id.map_or(AppRoute::Overview, AppRoute::Component))
        .collect()
}

struct Harness {
    ctx: Context,
    frame: u32,
    viewport: Vec2,
}

impl Harness {
    fn new(viewport: Vec2) -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
            viewport,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.step_with(Vec::new(), body)
    }

    fn step_with(
        &mut self,
        events: Vec<Event>,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, self.viewport)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

fn rendered_labels(out: &egui::FullOutput) -> Vec<String> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect()
}

#[test]
fn neighbors_follow_the_catalog_order() {
    let order = catalog_routes();
    let mut forward = order.first().cloned().unwrap_or(AppRoute::Overview);
    let mut visited = vec![forward.clone()];
    while let Some(next) = forward.neighbors().1 {
        assert!(
            !visited.contains(&next),
            "the catalog order must not repeat a screen: {next:?}"
        );
        visited.push(next.clone());
        forward = next;
    }
    assert_eq!(visited, order, "the dock must walk the catalog order");

    let mut backward = order.last().cloned().unwrap_or(AppRoute::Overview);
    let mut walked = vec![backward.clone()];
    while let Some(previous) = backward.neighbors().0 {
        assert!(
            !walked.contains(&previous),
            "the catalog order must not repeat a screen: {previous:?}"
        );
        walked.push(previous.clone());
        backward = previous;
    }
    walked.reverse();
    assert_eq!(walked, order, "the dock must walk back the catalog order");
}

#[test]
fn the_first_and_the_last_screen_have_one_neighbour() {
    let order = catalog_routes();
    let first = order.first().cloned().unwrap_or(AppRoute::Overview);
    let last = order.last().cloned().unwrap_or(AppRoute::Overview);
    assert_eq!(first.neighbors().0, None, "the overview has no previous");
    assert_eq!(
        first.neighbors().1,
        Some(AppRoute::Component(ComponentId::Button))
    );
    assert_eq!(
        last.neighbors().1,
        None,
        "the last screen has no next screen"
    );
    assert_eq!(
        last.neighbors().0,
        Some(AppRoute::Component(ComponentId::WhiteLabel))
    );
}

#[test]
fn the_dock_renders_the_neighbour_screen_names() {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(ComponentId::ButtonGroup));
    let mut harness = Harness::new(Vec2::new(1280.0, 900.0));
    let mut body = |ui: &mut egui::Ui| {
        _ = app.show_screen_dock(ui, Language::Eng);
    };
    let labels = rendered_labels(&harness.step(&mut body));
    assert!(
        labels.contains(&"Button".to_owned()),
        "the dock must name the previous screen: {labels:?}"
    );
    assert!(
        labels.contains(&"Checkbox".to_owned()),
        "the dock must name the next screen: {labels:?}"
    );
}

#[test]
fn the_dock_hides_the_side_without_a_neighbour() {
    let mut overview_app = ShowcaseApp::default();
    let mut overview_harness = Harness::new(Vec2::new(1280.0, 900.0));
    let overview_dock = std::cell::RefCell::new(None);
    {
        let mut body = |ui: &mut egui::Ui| {
            *overview_dock.borrow_mut() = Some(overview_app.show_screen_dock(ui, Language::Eng));
        };
        let _ = overview_harness.step(&mut body);
    }
    let (overview_has_previous, overview_has_next) = overview_dock.borrow().as_ref().map_or_else(
        || panic!("the dock must render on the overview"),
        |dock| (dock.previous.is_some(), dock.next.is_some()),
    );
    assert!(
        !overview_has_previous,
        "the overview has no previous screen to show"
    );
    assert!(overview_has_next, "the overview must show the next screen");

    let mut last_app = ShowcaseApp::default();
    last_app.navigate_to(Some(ComponentId::Utilities));
    let mut last_harness = Harness::new(Vec2::new(1280.0, 900.0));
    let last_dock = std::cell::RefCell::new(None);
    {
        let mut body = |ui: &mut egui::Ui| {
            *last_dock.borrow_mut() = Some(last_app.show_screen_dock(ui, Language::Eng));
        };
        let _ = last_harness.step(&mut body);
    }
    let (last_has_previous, last_has_next) = last_dock.borrow().as_ref().map_or_else(
        || panic!("the dock must render on the last screen"),
        |dock| (dock.previous.is_some(), dock.next.is_some()),
    );
    assert!(!last_has_next, "the last screen has no next screen to show");
    assert!(
        last_has_previous,
        "the last screen must show the previous screen"
    );
}

#[test]
fn the_dock_navigates_to_the_next_screen() {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(ComponentId::ButtonGroup));
    let mut harness = Harness::new(Vec2::new(1280.0, 900.0));
    let next_rect = std::cell::Cell::new(None);
    {
        let mut body = |ui: &mut egui::Ui| {
            let dock = app.show_screen_dock(ui, Language::Eng);
            next_rect.set(dock.next.as_ref().map(|response| response.rect));
        };
        let _ = harness.step(&mut body);
        let Some(rect) = next_rect.get() else {
            panic!("the next button must be rendered");
        };
        let center = rect.center();
        let _ = harness.step_with(vec![Event::PointerMoved(center)], &mut body);
        let _ = harness.step_with(
            vec![Event::PointerButton {
                pos: center,
                button: egui::PointerButton::Primary,
                pressed: true,
                modifiers: egui::Modifiers::default(),
            }],
            &mut body,
        );
        let _ = harness.step_with(
            vec![Event::PointerButton {
                pos: center,
                button: egui::PointerButton::Primary,
                pressed: false,
                modifiers: egui::Modifiers::default(),
            }],
            &mut body,
        );
    }
    assert_eq!(
        app.selected,
        Some(ComponentId::Checkbox),
        "clicking next must open the next screen"
    );
    assert_eq!(
        app.router.current().component(),
        Some(ComponentId::Checkbox),
        "clicking next must move the router"
    );
}

fn dock_buttons(viewport: Vec2) -> (egui::Rect, egui::Rect) {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(ComponentId::ButtonGroup));
    let mut harness = Harness::new(viewport);
    let rendered_dock = std::cell::RefCell::new(None);
    {
        let mut body = |ui: &mut egui::Ui| {
            *rendered_dock.borrow_mut() = Some(app.show_screen_dock(ui, Language::Eng));
        };
        let _ = harness.step(&mut body);
    }
    let borrowed_dock = rendered_dock.borrow();
    let dock = borrowed_dock
        .as_ref()
        .unwrap_or_else(|| panic!("the dock must render"));
    let previous = dock
        .previous
        .as_ref()
        .unwrap_or_else(|| panic!("the previous button must be rendered"))
        .rect;
    let next = dock
        .next
        .as_ref()
        .unwrap_or_else(|| panic!("the next button must be rendered"))
        .rect;
    (previous, next)
}

#[test]
fn the_dock_lays_the_buttons_out_side_by_side_on_desktop() {
    let (previous, next) = dock_buttons(Vec2::new(1280.0, 900.0));
    assert!(
        previous.right() <= next.left(),
        "the previous button must stay left of the next one: {previous:?} {next:?}"
    );
    assert!(
        (previous.center().y - next.center().y).abs() < 1.0,
        "both buttons must share a row: {previous:?} {next:?}"
    );
}

#[test]
fn the_dock_stacks_the_buttons_on_mobile() {
    let (previous, next) = dock_buttons(Vec2::new(390.0, 900.0));
    assert!(
        previous.bottom() <= next.top(),
        "the buttons must stack on a narrow screen: {previous:?} {next:?}"
    );
}

#[test]
fn every_screen_renders_the_dock() {
    let order = catalog_routes();
    for (index, route) in order.iter().enumerate() {
        let mut screen_app = ShowcaseApp::default();
        screen_app.navigate_to(route.component());
        let mut screen_harness = Harness::new(Vec2::new(1280.0, 900.0));
        let rendered_dock = std::cell::RefCell::new(None);
        {
            let mut body = |ui: &mut egui::Ui| {
                *rendered_dock.borrow_mut() = Some(screen_app.render_component(ui, Language::Eng));
            };
            let _ = screen_harness.step(&mut body);
        }
        let borrowed_dock = rendered_dock.borrow();
        let dock = borrowed_dock
            .as_ref()
            .unwrap_or_else(|| panic!("the {route} screen must render the dock"));
        assert_eq!(
            dock.previous.is_some(),
            index > 0,
            "the {route} screen must dock a button to the previous screen"
        );
        assert_eq!(
            dock.next.is_some(),
            index + 1 < order.len(),
            "the {route} screen must dock a button to the next screen"
        );
    }
}

#[test]
fn the_dock_is_the_last_thing_of_a_screen() {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(ComponentId::ButtonGroup));
    let mut harness = Harness::new(Vec2::new(1280.0, 900.0));
    let content_bottom = std::cell::Cell::new(None);
    let dock_bottom = std::cell::Cell::new(None);
    {
        let mut body = |ui: &mut egui::Ui| {
            let dock = app.render_component(ui, Language::Eng);
            content_bottom.set(Some(ui.min_rect().bottom()));
            dock_bottom.set(dock.next.as_ref().map(|response| response.rect.bottom()));
        };
        let _ = harness.step(&mut body);
    }
    let page_bottom = content_bottom
        .get()
        .unwrap_or_else(|| panic!("the screen must be rendered"));
    let button_bottom = dock_bottom
        .get()
        .unwrap_or_else(|| panic!("the next button must be rendered"));
    assert!(
        page_bottom - button_bottom <= 24.0,
        "the dock must be the last thing of the screen: page {page_bottom}, dock {button_bottom}"
    );
}
