#![cfg(feature = "clipboard")]

//! Two paste-clear widgets in the same parent must not share their inner
//! ids: the white-label donate screen renders one widget per crypto address,
//! and every widget needs its own scroll area and text edit state.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

fn rendered_texts(output: &egui::FullOutput) -> Vec<String> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect()
}

fn render(mut body: impl FnMut(&mut egui::Ui)) -> Vec<String> {
    let ctx = Context::default();
    ctx.options_mut(|options| options.warn_on_id_clash = true);
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        },
    );
    out.textures_delta.clear();
    rendered_texts(&out)
}

fn clashes(texts: &[String]) -> Vec<&String> {
    texts
        .iter()
        .filter(|text| text.contains("use of ScrollArea") || text.contains("use of TextEdit"))
        .collect()
}

#[test]
fn two_textareas_in_one_parent_do_not_clash_ids() {
    let mut first = "first address".to_owned();
    let mut second = "second address".to_owned();
    let texts = render(|ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut first)
            .min_height(80.0)
            .show(ui);
        let _ = functora_egui::TextareaPasteClear::new(&mut second)
            .min_height(80.0)
            .show(ui);
    });
    assert!(
        clashes(&texts).is_empty(),
        "two textareas in one parent must keep their own ids: {texts:?}"
    );
}

#[test]
fn two_inputs_in_one_parent_do_not_clash_ids() {
    let mut first = "first address".to_owned();
    let mut second = "second address".to_owned();
    let texts = render(|ui| {
        let _ = functora_egui::InputPasteClear::new(&mut first).show(ui);
        let _ = functora_egui::InputPasteClear::new(&mut second).show(ui);
    });
    assert!(
        clashes(&texts).is_empty(),
        "two inputs in one parent must keep their own ids: {texts:?}"
    );
}
