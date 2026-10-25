use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

fn content() -> String {
    "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q-".repeat(24)
}

fn run_with(
    ctx: &Context,
    frame: u32,
    events: Vec<Event>,
    body: &mut dyn FnMut(&mut egui::Ui),
) -> egui::FullOutput {
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        time: Some(f64::from(frame) / 60.0),
        events,
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |panel_ui| {
            let _ = egui::ScrollArea::vertical().show(panel_ui, |scroll_ui| body(scroll_ui));
        });
    });
    out.textures_delta.clear();
    out
}

fn border_heights(output: &egui::FullOutput) -> Vec<f32> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if rect.stroke.width > 0.0 => Some(rect.rect.height()),
            _ => None,
        })
        .collect()
}

fn border_boxes(output: &egui::FullOutput) -> Vec<Rect> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if rect.stroke.width > 0.0 => Some(rect.rect),
            _ => None,
        })
        .collect()
}

fn marker_center(output: &egui::FullOutput) -> Pos2 {
    let mut found = Vec::new();
    for clipped in &output.shapes {
        if let Shape::Text(text) = &clipped.shape
            && text.galley.text() == "BELOW"
        {
            found.push(text.galley.rect.translate(text.pos.to_vec2()).center());
        }
    }
    found
        .into_iter()
        .next()
        .unwrap_or_else(|| panic!("BELOW marker must render"))
}

#[test]
#[cfg(feature = "clipboard")]
fn textarea_readonly_grows_to_fit_short_content() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut text = content();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text)
            .readonly()
            .min_height(80.0)
            .show(ui);
    };
    let out = run_with(&ctx, 2, Vec::new(), &mut body);
    let heights = border_heights(&out);
    let tallest = heights
        .iter()
        .copied()
        .reduce(f32::max)
        .unwrap_or_else(|| panic!("expected at least one border box"));
    assert!(
        tallest > 80.5,
        "readonly field must grow past its minimum to fit content, got {tallest}"
    );
    assert!(
        tallest < 250.0,
        "readonly field must stay compact for short content, got {tallest}"
    );
}

#[test]
#[cfg(feature = "clipboard")]
fn control_plain_area_wheel_scrolls_page() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut frame = 100u32;
    let mut body = |ui: &mut egui::Ui| {
        ui.add_space(300.0);
        let _ = ui.label("BELOW");
        ui.add_space(2000.0);
    };
    let mut page = |events: Vec<Event>| {
        frame += 1;
        run_with(&ctx, frame, events, &mut body)
    };
    let wheel = |delta_y: f32| Event::MouseWheel {
        unit: egui::MouseWheelUnit::Point,
        delta: Vec2::new(0.0, delta_y),
        phase: egui::TouchPhase::Move,
        modifiers: egui::Modifiers::default(),
    };
    let before = marker_center(&page(vec![]));
    let _ = page(vec![Event::PointerMoved(before)]);
    let _ = page(vec![wheel(-30.0)]);
    for _ in 0..10 {
        let _ = page(vec![]);
    }
    let after = marker_center(&page(vec![]));
    assert!(
        after.y + 2.0 < before.y,
        "page must scroll on wheel over plain content, marker {before:?} -> {after:?}"
    );
}

#[test]
#[cfg(feature = "clipboard")]
fn textarea_readonly_renders_muted_background() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    functora_egui::theme_extra::set_theme(&ctx, functora_egui::theme_extra::Theme::Dark);
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let mut readonly_text = "READONLY-CONTENT-0123456789".to_owned();
    let mut editable_text = "EDITABLE-CONTENT-0123456789".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut readonly_text)
            .readonly()
            .min_height(80.0)
            .show(ui);
        let _ = functora_egui::TextareaPasteClear::new(&mut editable_text)
            .min_height(80.0)
            .show(ui);
    };
    let out = run_with(&ctx, 2, Vec::new(), &mut body);
    let fills: Vec<(f32, egui::Color32)> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if (rect.rect.height() - 80.0).abs() <= 1.0 => {
                Some((rect.rect.min.y, rect.fill))
            }
            _ => None,
        })
        .collect();
    let muted_y = fills
        .iter()
        .find(|(_, color)| *color == dark.muted)
        .map_or_else(
            || panic!("readonly field must render a muted inactive background"),
            |(y, _)| *y,
        );
    let plain_y = fills
        .iter()
        .find(|(_, color)| *color == dark.background)
        .map_or_else(
            || panic!("editable field must keep the default background"),
            |(y, _)| *y,
        );
    assert!(
        muted_y < plain_y,
        "muted box must belong to the first (readonly) field"
    );
    let mut readonly_colors = Vec::new();
    let mut editable_colors = Vec::new();
    for clipped in &out.shapes {
        if let Shape::Text(text) = &clipped.shape {
            if text.galley.text() == readonly_text {
                readonly_colors.extend(text.galley.job.sections.iter().map(|s| s.format.color));
            }
            if text.galley.text() == editable_text {
                editable_colors.extend(text.galley.job.sections.iter().map(|s| s.format.color));
            }
        }
    }
    assert!(!readonly_colors.is_empty(), "readonly content must render");
    assert!(
        readonly_colors.iter().all(|c| *c == dark.foreground),
        "readonly text must stay fully readable, got {readonly_colors:?}"
    );
    assert!(!editable_colors.is_empty(), "editable content must render");
    assert!(
        editable_colors.iter().all(|c| *c == dark.foreground),
        "editable text must render in full foreground, got {editable_colors:?}"
    );
}

#[test]
#[cfg(feature = "clipboard")]
fn textarea_wheel_reaches_page_when_content_fits() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut text = content();
    let mut frame = 2u32;
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text)
            .readonly()
            .min_height(80.0)
            .show(ui);
        let _ = ui.label("BELOW");
        ui.add_space(2000.0);
    };
    let wheel = |delta_y: f32| Event::MouseWheel {
        unit: egui::MouseWheelUnit::Point,
        delta: Vec2::new(0.0, delta_y),
        phase: egui::TouchPhase::Move,
        modifiers: egui::Modifiers::default(),
    };
    let mut page = |events: Vec<Event>| {
        frame += 1;
        run_with(&ctx, frame, events, &mut body)
    };
    let field = border_boxes(&page(Vec::new()))
        .into_iter()
        .reduce(|a, b| if a.height() > b.height() { a } else { b })
        .unwrap_or_else(|| panic!("field box must render"));
    let center = field.center();
    let before = marker_center(&page(vec![Event::PointerMoved(center)]));
    let _ = page(vec![wheel(-30.0)]);
    for _ in 0..10 {
        let _ = page(vec![]);
    }
    let out = page(vec![]);
    let after = marker_center(&out);
    assert!(
        after.y + 2.0 < before.y,
        "page must scroll when wheeling over a fitted field, marker {before:?} -> {after:?}"
    );
}
