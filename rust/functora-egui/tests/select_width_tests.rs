//! Select trigger must grow to fit long labels when space allows,
//! instead of sticking to a fixed 200px cap and overflowing.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use std::cell::Cell;
use std::rc::Rc;

const DESKTOP: Vec2 = Vec2::new(1280.0, 800.0);

fn run_frames(
    ctx: &Context,
    screen: Vec2,
    frames: u32,
    mut frame: impl FnMut(&mut egui::Ui),
) -> egui::FullOutput {
    (0..frames)
        .map(|idx| {
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
                time: Some(f64::from(idx) / 60.0),
                ..Default::default()
            };
            let mut out = ctx.run_ui(raw, |ui| frame(ui));
            out.textures_delta.clear();
            out
        })
        .last()
        .unwrap_or_default()
}

fn cipher_entries() -> [(u8, String); 3] {
    [
        (0, "No encryption (plaintext)".to_owned()),
        (1, "AES-256-GCM (encryption)".to_owned()),
        (2, "ChaCha20-Poly1305 (encryption)".to_owned()),
    ]
}

#[test]
fn select_value_labeled_grows_to_fit_long_labels_on_desktop() {
    let ctx = Context::default();
    let all = cipher_entries();
    let trigger_width: Rc<Cell<f32>> = Rc::new(Cell::new(0.0));
    let available: Rc<Cell<f32>> = Rc::new(Cell::new(0.0));
    let width_clone = trigger_width.clone();
    let avail_clone = available.clone();
    let mut selected: u8 = 2;
    let _ = run_frames(&ctx, DESKTOP, 1, |ui| {
        avail_clone.set(ui.available_width());
        let resp = functora_egui::SelectValueLabeled::new(&mut selected, &all).show(ui);
        width_clone.set(resp.rect.width());
    });
    let width = trigger_width.get();
    let avail = available.get();
    assert!(
        width <= avail + 0.5,
        "trigger must never exceed available width, got {width} > {avail}"
    );
    assert!(
        width > 200.0,
        "trigger must grow beyond the legacy 200px cap to fit long cipher labels, got {width}"
    );
}

#[test]
fn select_labeled_grows_to_fit_long_labels_on_desktop() {
    let ctx = Context::default();
    let all = cipher_entries();
    let trigger_width: Rc<Cell<f32>> = Rc::new(Cell::new(0.0));
    let width_clone = trigger_width.clone();
    let mut selected: Option<u8> = Some(2);
    let _ = run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = functora_egui::SelectLabeled::new(&mut selected, &all).show(ui);
        width_clone.set(resp.rect.width());
    });
    assert!(
        trigger_width.get() > 200.0,
        "SelectLabeled must grow beyond 200px for long labels, got {}",
        trigger_width.get()
    );
}

#[test]
fn combobox_value_grows_to_fit_long_labels_on_desktop() {
    let ctx = Context::default();
    let all = cipher_entries();
    let trigger_width: Rc<Cell<f32>> = Rc::new(Cell::new(0.0));
    let width_clone = trigger_width.clone();
    let mut selected: Option<u8> = Some(2);
    let mut search = String::new();
    let _ = run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = functora_egui::ComboboxValue::new(&all).show(ui, &mut selected, &mut search);
        width_clone.set(resp.rect.width());
    });
    assert!(
        trigger_width.get() > 220.0,
        "ComboboxValue must grow beyond the legacy 220px cap, got {}",
        trigger_width.get()
    );
}

#[test]
fn select_trigger_never_exceeds_narrow_container() {
    let ctx = Context::default();
    let all = cipher_entries();
    let trigger_width: Rc<Cell<f32>> = Rc::new(Cell::new(0.0));
    let width_clone = trigger_width.clone();
    let mut selected: u8 = 2;
    let _ = run_frames(&ctx, DESKTOP, 1, |ui| {
        let _ = ui.horizontal(|inner| {
            inner.set_max_width(150.0);
            let resp = functora_egui::SelectValueLabeled::new(&mut selected, &all).show(inner);
            width_clone.set(resp.rect.width());
        });
    });
    assert!(
        trigger_width.get() <= 150.5,
        "trigger must shrink to a narrow container instead of overflowing, got {}",
        trigger_width.get()
    );
}

fn collect_texts(output: &egui::FullOutput) -> Vec<(String, egui::Rect)> {
    fn walk(shape: &egui::Shape, out: &mut Vec<(String, egui::Rect)>) {
        match shape {
            egui::Shape::Text(text) => out.push((
                text.galley.text().to_owned(),
                text.galley.rect.translate(text.pos.to_vec2()),
            )),
            egui::Shape::Vec(shapes) => {
                for inner in shapes {
                    walk(inner, out);
                }
            }
            _ => {}
        }
    }
    let mut found = Vec::new();
    for clipped in &output.shapes {
        walk(&clipped.shape, &mut found);
    }
    found
}

#[test]
fn select_value_labeled_renders_identically_to_select_labeled() {
    let all = cipher_entries();

    let value_ctx = Context::default();
    let mut value_selected: u8 = 2;
    let value_rect: Rc<Cell<egui::Rect>> = Rc::new(Cell::new(egui::Rect::NOTHING));
    let value_rect_slot = value_rect.clone();
    let value_out = run_frames(&value_ctx, DESKTOP, 1, |ui| {
        let resp = functora_egui::SelectValueLabeled::new(&mut value_selected, &all).show(ui);
        value_rect_slot.set(resp.rect);
    });

    let labeled_ctx = Context::default();
    let mut labeled_selected: Option<u8> = Some(2);
    let labeled_rect: Rc<Cell<egui::Rect>> = Rc::new(Cell::new(egui::Rect::NOTHING));
    let labeled_rect_slot = labeled_rect.clone();
    let labeled_out = run_frames(&labeled_ctx, DESKTOP, 1, |ui| {
        let resp = functora_egui::SelectLabeled::new(&mut labeled_selected, &all).show(ui);
        labeled_rect_slot.set(resp.rect);
    });

    assert_eq!(
        value_rect.get(),
        labeled_rect.get(),
        "SelectValueLabeled must occupy the same trigger rect as SelectLabeled"
    );
    assert_eq!(
        collect_texts(&value_out),
        collect_texts(&labeled_out),
        "SelectValueLabeled must paint the same text as SelectLabeled"
    );
    assert_eq!(
        value_selected, 2,
        "selection must stay untouched while the popup stays closed"
    );
}
