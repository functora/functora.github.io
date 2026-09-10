//! Image sizing regression: fractional sizing must stay bounded in wide
//! viewports, and tilted images must stay compact instead of overflowing
//! their layout box into neighboring content.

use egui::{Context, Pos2, RawInput, Rect, Vec2};

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        let ctx = Context::default();
        functora_egui::setup_image_loaders(&ctx);
        Self { ctx, frame: 0 }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        // Tall viewport: fractional sizing must stay bounded even when the
        // available height is effectively unbounded (scrollable content).
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 6000.0))),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

fn sample_png() -> Vec<u8> {
    match functora_egui::image_samples() {
        Ok(samples) => samples.png.bytes.clone(),
        Err(error) => panic!("fixtures must encode: {error:?}"),
    }
}

/// Image decoding runs on a background thread, so poll frames until the
/// expected shapes show up (bounded wall-clock budget, then assert).
fn poll_until(
    harness: &mut Harness,
    body: &mut dyn FnMut(&mut egui::Ui),
    ready: impl Fn(&egui::FullOutput) -> bool,
) -> egui::FullOutput {
    let mut out = harness.step(body);
    for _ in 0..200 {
        if ready(&out) {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
        out = harness.step(body);
    }
    out
}

fn textured_rects(out: &egui::FullOutput) -> Vec<Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect) if rect.fill_texture_id() != egui::TextureId::default() => {
                Some(rect.rect)
            }
            _ => None,
        })
        .collect()
}

fn mesh_bounds(out: &egui::FullOutput) -> Vec<Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Mesh(mesh) => Some(mesh.calc_bounds()),
            _ => None,
        })
        .collect()
}

#[test]
fn fraction_images_stay_bounded_on_wide_viewports() {
    let mut harness = Harness::new();
    let bytes = sample_png();
    // Mirrors the fit_to_fraction / shrink_to_fit demo: fractions of the
    // available space, capped so wide viewports stay sane.
    let mut body = |ui: &mut egui::Ui| {
        _ = ui.add(
            egui::Image::from_bytes("bytes://frac.png", bytes.clone())
                .fit_to_fraction(egui::vec2(0.5, 0.5))
                .max_size(egui::vec2(280.0, 180.0)),
        );
        _ = ui.add(
            egui::Image::from_bytes("bytes://shrink.png", bytes.clone())
                .shrink_to_fit()
                .max_size(egui::vec2(320.0, 200.0)),
        );
    };
    let out = poll_until(&mut harness, &mut body, |out| {
        textured_rects(out).len() == 2
    });
    let rects = textured_rects(&out);
    assert_eq!(rects.len(), 2, "both fraction images must render");
    for rect in &rects {
        assert!(
            rect.height() <= 400.0,
            "fraction images must stay bounded, got {rects:?}",
        );
    }
}

#[test]
fn rotated_image_stays_compact() {
    let mut harness = Harness::new();
    let bytes = sample_png();
    // Mirrors the rotate demo: tilted corners extend past the layout box,
    // so the displayed size must leave room for them.
    let mut body = |ui: &mut egui::Ui| {
        _ = ui.add(
            egui::Image::from_bytes("bytes://spin.png", bytes.clone())
                .rotate(std::f32::consts::FRAC_PI_4, egui::vec2(0.5, 0.5))
                .fit_to_exact_size(egui::vec2(80.0, 80.0)),
        );
    };
    let out = poll_until(&mut harness, &mut body, |out| mesh_bounds(out).len() == 1);
    let bounds = mesh_bounds(&out);
    assert_eq!(bounds.len(), 1, "rotated image must render");
    assert!(
        bounds[0].width() <= 130.0,
        "rotated image must stay compact, got {:?}",
        bounds[0],
    );
}

fn assert_size(label: &str, got: egui::Vec2, want: egui::Vec2) {
    assert!(
        (got.x - want.x).abs() < 0.02 && (got.y - want.y).abs() < 0.02,
        "{label} must size to {want:?} regardless of available space, got {got:?}",
    );
}

#[test]
fn demo_images_size_without_available_height() {
    // The Shell nests content as scroll > horizontal spacers > vertical, so
    // direct children measure against ~zero available height (observed
    // [1022.9 0.0] in the browser). Bare images must size independently of
    // it; Flex tiles measure unbounded instead.
    let shell = egui::vec2(1022.0, 0.0);
    let loaded = Some(egui::vec2(256.0, 256.0));
    let bytes = Vec::<u8>::new();
    assert_size(
        "sense",
        egui::Image::from_bytes("bytes://sense.png", bytes.clone())
            .sense(egui::Sense::click())
            .fit_to_exact_size(egui::vec2(120.0, 120.0))
            .calc_size(shell, loaded),
        egui::vec2(120.0, 120.0),
    );
    assert_size(
        "rotate",
        egui::Image::from_bytes("bytes://spin.png", bytes.clone())
            .rotate(std::f32::consts::FRAC_PI_4, egui::vec2(0.5, 0.5))
            .fit_to_exact_size(egui::vec2(80.0, 80.0))
            .calc_size(shell, loaded),
        egui::vec2(80.0, 80.0),
    );
    assert_size(
        "responsive desktop",
        egui::Image::from_bytes("bytes://wide.png", bytes.clone())
            .maintain_aspect_ratio(true)
            .fit_to_exact_size(egui::vec2(400.0, 400.0))
            .calc_size(shell, loaded),
        egui::vec2(400.0, 400.0),
    );
    assert_size(
        "responsive mobile",
        egui::Image::from_bytes("bytes://wide.png", bytes.clone())
            .maintain_aspect_ratio(true)
            .fit_to_exact_size(egui::vec2(160.0, 160.0))
            .calc_size(shell, loaded),
        egui::vec2(160.0, 160.0),
    );
    assert_size(
        "error",
        egui::Image::from_bytes("bytes://missing.png", bytes.clone())
            .maintain_aspect_ratio(false)
            .fit_to_exact_size(egui::vec2(300.0, 64.0))
            .calc_size(shell, loaded),
        egui::vec2(300.0, 64.0),
    );
    let roomy = egui::vec2(2000.0, 2000.0);
    assert_size(
        "fit_to_fraction tile",
        egui::Image::from_bytes("bytes://frac.png", bytes.clone())
            .fit_to_fraction(egui::vec2(0.5, 0.5))
            .max_size(egui::vec2(280.0, 180.0))
            .calc_size(roomy, loaded),
        egui::vec2(180.0, 180.0),
    );
    assert_size(
        "shrink_to_fit tile",
        egui::Image::from_bytes("bytes://shrink.png", bytes.clone())
            .shrink_to_fit()
            .max_size(egui::vec2(320.0, 200.0))
            .calc_size(roomy, loaded),
        egui::vec2(200.0, 200.0),
    );
}
