//! Layout regression: no demo screen may overflow the viewport.
//!
//! The Flex, Camera and `QrScanner` screens used to render wider than the
//! page column on a phone: the no-wrap badge row, the fixed camera preview
//! and the fixed scanner preview all ran past the right edge.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::i18n::Language;
use functora_egui_demo::route::AppRoute;
use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

const MOBILE_SIZE: Vec2 = Vec2::new(390.0, 844.0);
const DESKTOP_SIZE: Vec2 = Vec2::new(1280.0, 900.0);

struct Measurement {
    content: Rect,
    column: Rect,
}

fn measure(id: ComponentId, viewport: Vec2) -> Measurement {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(id));
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, viewport)),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let measured = std::cell::RefCell::new(None);
    let mut out = ctx.run_ui(raw, |ui| {
        let mut collapsed = false;
        let _ = functora_egui::Shell::<AppRoute>::new("test", &mut collapsed, |_| false).show(
            ui,
            |content_ui| {
                let column = content_ui.max_rect();
                let _ = app.render_component(content_ui, Language::Eng);
                *measured.borrow_mut() = Some((content_ui.min_rect(), column));
            },
        );
    });
    out.textures_delta.clear();
    let (content, column) = measured
        .borrow()
        .as_ref()
        .copied()
        .unwrap_or_else(|| panic!("the screen must be rendered"));
    Measurement { content, column }
}

fn screen_ids() -> Vec<ComponentId> {
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .filter_map(|def| def.id)
        .collect()
}

#[test]
fn no_screen_overflows_the_mobile_viewport() {
    for id in screen_ids() {
        let measured = measure(id, MOBILE_SIZE);
        assert!(
            measured.content.right() <= MOBILE_SIZE.x,
            "the {id:?} screen must not run past the mobile viewport: {:?}",
            measured.content
        );
    }
}

#[test]
fn no_screen_overflows_the_desktop_viewport() {
    for id in screen_ids() {
        let measured = measure(id, DESKTOP_SIZE);
        assert!(
            measured.content.right() <= DESKTOP_SIZE.x,
            "the {id:?} screen must not run past the desktop viewport: {:?}",
            measured.content
        );
    }
}

#[test]
fn the_wide_screens_fit_the_mobile_page_column() {
    for id in [
        ComponentId::Flex,
        ComponentId::Camera,
        ComponentId::QrScanner,
        ComponentId::QrImage,
    ] {
        let measured = measure(id, MOBILE_SIZE);
        assert!(
            measured.content.right() <= measured.column.right() + 0.5,
            "the {id:?} screen must stay inside the page column: {:?} vs {:?}",
            measured.content,
            measured.column
        );
    }
}
