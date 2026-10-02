//! Each demo's teaching snippet must describe what the live demo
//! actually renders: same widgets, same APIs, same labels.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui_demo::ShowcaseApp;

fn rendered_text<F: FnMut(&mut egui::Ui)>(mut body: F) -> String {
    let ctx = Context::default();
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 900.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        },
    );
    out.textures_delta.clear();
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn rendered_app<F>(mut body: F) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let mut app = ShowcaseApp::default();
    rendered_text(|ui| body(&mut app, ui))
}

#[test]
fn input_paste_clear_teaches_rendered_combo() {
    let text = rendered_app(ShowcaseApp::demo_input_paste_clear);
    assert!(
        text.contains("Password + custom icons"),
        "live demo must render the password+icons combo section: {text}"
    );
    assert!(
        text.contains(".password()") && text.contains(".paste_icon("),
        "snippet must teach the rendered password+icons combo: {text}"
    );
}

#[test]
fn breadcrumb_snippet_teaches_separator() {
    let text = rendered_app(ShowcaseApp::demo_breadcrumb);
    assert!(
        text.contains("Custom separator"),
        "live demo must render the separator section: {text}"
    );
    assert!(
        text.contains(".separator("),
        "snippet must teach the separator API: {text}"
    );
}

#[test]
fn flex_snippet_covers_rendered_modes() {
    let text = rendered_app(ShowcaseApp::demo_flex);
    for needle in [
        "Justify end",
        "Justify center",
        "Nested flex",
        "Center utility",
        ".justify_end()",
        ".justify_center()",
        ".grow_nested(",
        "center(ui,",
    ] {
        assert!(
            text.contains(needle),
            "flex demo/snippet must cover `{needle}`: {text}"
        );
    }
}

#[test]
fn field_group_snippet_month_compiles_conceptually() {
    let text = rendered_app(ShowcaseApp::demo_field_group);
    assert!(
        text.contains("Groups related fields"),
        "live demo must render its intro: {text}"
    );
    assert!(
        text.contains("#[derive(Debug, Clone, Copy, PartialEq, Eq)]")
            && text.contains("struct Month(u8);"),
        "snippet Month must carry the Clone+PartialEq derives SelectLabeled requires: {text}"
    );
}

#[test]
fn toast_snippet_uses_inner_click_and_long_example() {
    let text = rendered_app(ShowcaseApp::demo_toast);
    assert!(
        text.contains("Transient notifications with variants"),
        "live demo must render its intro: {text}"
    );
    assert!(
        text.contains(".inner.clicked()"),
        "toast snippet must use .inner.clicked() like Flex::add requires: {text}"
    );
    assert!(
        text.contains("Sync completed with a very long multiline title"),
        "toast snippet must include the long multiline example: {text}"
    );
}

#[test]
fn navbar_snippet_teaches_callbacks() {
    let text = rendered_app(ShowcaseApp::demo_navbar);
    assert!(
        text.contains("language switcher"),
        "live demo must render its intro: {text}"
    );
    assert!(
        text.contains("Some(&mut on_brand)"),
        "navbar snippet must wire brand/search callbacks like live: {text}"
    );
}

#[test]
fn files_snippet_matches_shared_progress_flow() {
    let text = rendered_app(ShowcaseApp::demo_files);
    assert!(
        text.contains("Preview via"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains("pick_files_with_shared_progress"),
        "files snippet must teach the live shared-progress flow: {text}"
    );
    assert!(
        !text.contains("preview_cached"),
        "files snippet must not advertise APIs the demo never calls: {text}"
    );
}

#[test]
fn progress_worker_snippet_claims_option_slot() {
    let text = rendered_app(ShowcaseApp::demo_progress_worker);
    assert!(
        text.contains("Worker::run"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains("Option<Job<Stage>>"),
        "snippet must claim an Option slot like claim_job requires: {text}"
    );
}

#[test]
fn qr_scanner_labels_its_fixture() {
    let text = rendered_app(ShowcaseApp::demo_qr_scanner);
    assert!(
        text.contains("QrScanner widget"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains("Scan target"),
        "scanner demo must label its QrImage as a scan fixture: {text}"
    );
}

#[test]
fn package_snippet_uses_demo_env_names() {
    let text = rendered_text(ShowcaseApp::demo_package);
    assert!(
        text.contains("crate: "),
        "live demo must render its package line: {text}"
    );
    assert!(
        text.contains("DEMO_WEB_TITLE"),
        "package snippet must use the real build.rs env names: {text}"
    );
    assert!(
        !text.contains("MY_TITLE"),
        "package snippet must not use placeholder env names: {text}"
    );
}

#[test]
fn hypertext_snippet_uses_year_constant() {
    let text = rendered_app(ShowcaseApp::demo_hypertext);
    assert!(
        text.contains("internal action segments"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains("FUNCTORA_CORE_YEAR"),
        "hypertext snippet must use the year constant like live: {text}"
    );
}

#[test]
fn kbd_snippet_uses_flex_ui_blocks() {
    let text = rendered_text(ShowcaseApp::demo_kbd);
    assert!(
        text.contains("Keyboard hint chips"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains("f.ui(|ui|"),
        "kbd snippet must use f.ui blocks like live (Flex has no .label): {text}"
    );
}

#[test]
fn alert_snippet_covers_all_live_variants() {
    let text = rendered_text(ShowcaseApp::demo_alert);
    assert!(
        text.contains("A status message container"),
        "live demo must render its description: {text}"
    );
    for needle in [
        "AlertVariant::Default",
        "AlertVariant::Destructive",
        "AlertVariant::Success",
        "AlertVariant::Warning",
        "AlertVariant::Info",
    ] {
        assert!(
            text.contains(needle),
            "alert snippet must cover `{needle}`: {text}"
        );
    }
}

#[test]
fn sheet_snippet_covers_all_sides() {
    let text = rendered_app(ShowcaseApp::demo_sheet);
    assert!(
        text.contains("slides in from the edge"),
        "live demo must render its description: {text}"
    );
    for needle in [
        "SheetSide::Right",
        "SheetSide::Left",
        "SheetSide::Top",
        "SheetSide::Bottom",
    ] {
        assert!(
            text.contains(needle),
            "sheet demo/snippet must cover `{needle}`: {text}"
        );
    }
}

#[test]
fn tabs_snippet_matches_fill_width_demo() {
    let text = rendered_app(ShowcaseApp::demo_tabs);
    assert!(
        text.contains("Tabbed content panels."),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains(".fill_width()"),
        "tabs snippet must teach fill_width: {text}"
    );
    assert!(
        text.contains("Equal-width account tab."),
        "tabs snippet must match live fill_width content: {text}"
    );
}

#[test]
fn card_snippet_teaches_heading() {
    let text = rendered_text(ShowcaseApp::demo_card);
    assert!(
        text.contains("Bordered container"),
        "live demo must render its description: {text}"
    );
    assert!(
        text.contains(".heading("),
        "card snippet must teach the real heading() API: {text}"
    );
    assert!(
        !text.contains(".header("),
        "card snippet must not advertise nonexistent header(): {text}"
    );
}

#[test]
fn touch_slider_state_is_isolated() {
    let mut app = ShowcaseApp {
        slider_val: 99.0,
        ..Default::default()
    };
    let ctx = Context::default();
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 900.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| app.demo_touch_target(inner));
        },
    );
    out.textures_delta.clear();
    assert!(
        (app.slider_val - 99.0).abs() < f64::EPSILON,
        "touch demo must not touch the input demo slider"
    );
    assert!(
        (app.touch_slider_val - 50.0).abs() < f64::EPSILON,
        "touch demo must use its own slider state"
    );
}
