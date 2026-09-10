//! Image grid regression: tiles with different caption lengths must keep
//! identical widths so images in a row stay column-aligned instead of
//! shifting with the text length.

use egui::{Context, Pos2, RawInput, Rect, Vec2};

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.step_with(Vec::new(), body)
    }

    fn step_with(
        &mut self,
        events: Vec<egui::Event>,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
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

fn tile_image_centers(out: &egui::FullOutput, texture: egui::TextureId) -> Vec<Pos2> {
    let mut centers: Vec<Pos2> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect) if rect.fill_texture_id() == texture => {
                Some(rect.rect.center())
            }
            _ => None,
        })
        .collect();
    centers.sort_by(|left, right| left.x.total_cmp(&right.x));
    centers
}

#[test]
fn image_tiles_keep_equal_spacing_despite_caption_length() {
    let mut harness = Harness::new();
    let texture = harness.ctx.load_texture(
        "tile-tex",
        egui::ColorImage::new(
            [48, 48],
            vec![egui::Color32::from_rgb(45, 120, 200); 48 * 48],
        ),
        egui::TextureOptions::LINEAR,
    );
    let texture_id = texture.id();
    let mut body = |ui: &mut egui::Ui| {
        _ = functora_egui::Flex::row().gap(8.0).show(ui, |f| {
            for caption in ["SVG", "Transparent PNG (alpha)", "Bottom-right quadrant"] {
                _ = f.ui(|tile| {
                    _ = functora_egui_demo::ShowcaseApp::image_tile(
                        tile,
                        160.0,
                        caption,
                        egui::Image::from_texture(egui::load::SizedTexture::from_handle(&texture)),
                    );
                });
            }
        });
    };
    let out = harness.step(&mut body);
    let centers = tile_image_centers(&out, texture_id);
    assert_eq!(centers.len(), 3, "all three tiles must render");
    let gaps: Vec<f32> = centers
        .windows(2)
        .map(|pair| pair[1].x - pair[0].x)
        .collect();
    assert!(
        (gaps[0] - gaps[1]).abs() < 1.0,
        "tile spacing must not depend on caption length, got gaps {gaps:?} for centers {centers:?}",
    );
}

#[test]
fn image_tiles_keep_top_alignment_despite_caption_wrap() {
    let mut harness = Harness::new();
    let texture = harness.ctx.load_texture(
        "tile-tex",
        egui::ColorImage::new(
            [48, 48],
            vec![egui::Color32::from_rgb(45, 120, 200); 48 * 48],
        ),
        egui::TextureOptions::LINEAR,
    );
    let texture_id = texture.id();
    // Mirrors the image grid rows: narrow tiles where the long caption wraps
    // onto extra lines, making its tile taller than its neighbor. Images must
    // stay top-aligned instead of shifting with the text height.
    let mut body = |ui: &mut egui::Ui| {
        _ = functora_egui::Flex::row()
            .gap(8.0)
            .align_start()
            .show(ui, |f| {
                for caption in ["SVG", "Transparent PNG (alpha) with extra words"] {
                    _ = f.ui(|tile| {
                        _ = functora_egui_demo::ShowcaseApp::image_tile(
                            tile,
                            96.0,
                            caption,
                            egui::Image::from_texture(egui::load::SizedTexture::from_handle(
                                &texture,
                            )),
                        );
                    });
                }
            });
    };
    let out = harness.step(&mut body);
    let centers = tile_image_centers(&out, texture_id);
    assert_eq!(centers.len(), 2, "both tiles must render");
    assert!(
        (centers[0].y - centers[1].y).abs() < 1.0,
        "images must stay top-aligned when a caption wraps, got centers {centers:?}",
    );
}

#[test]
fn sense_tile_click_registers_through_helper() {
    use std::cell::Cell;
    use std::rc::Rc;

    let mut harness = Harness::new();
    let texture = harness.ctx.load_texture(
        "tile-click-tex",
        egui::ColorImage::new(
            [48, 48],
            vec![egui::Color32::from_rgb(45, 120, 200); 48 * 48],
        ),
        egui::TextureOptions::LINEAR,
    );
    let rect_cell: Rc<Cell<Option<Rect>>> = Rc::new(Cell::new(None));
    let clicked_cell: Rc<Cell<bool>> = Rc::new(Cell::new(false));
    let mut body = {
        let tile_texture = texture.clone();
        let rect_slot = Rc::clone(&rect_cell);
        let clicked_flag = Rc::clone(&clicked_cell);
        move |ui: &mut egui::Ui| {
            _ = functora_egui::Flex::row().gap(8.0).show(ui, |f| {
                _ = f.ui(|tile| {
                    let response = functora_egui_demo::ShowcaseApp::image_tile(
                        tile,
                        160.0,
                        "",
                        egui::Image::from_texture(egui::load::SizedTexture::from_handle(
                            &tile_texture,
                        ))
                        .sense(egui::Sense::click()),
                    );
                    rect_slot.set(Some(response.rect));
                    if response.clicked() {
                        clicked_flag.set(true);
                    }
                });
            });
        }
    };
    let _ = harness.step(&mut body);
    let Some(rect) = rect_cell.get() else {
        panic!("tile image rect not found");
    };
    let center = rect.center();
    let click = |pressed| {
        vec![egui::Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed,
            modifiers: egui::Modifiers::default(),
        }]
    };
    let _ = harness.step_with(click(true), &mut body);
    let _ = harness.step_with(click(false), &mut body);
    assert!(
        clicked_cell.get(),
        "click on the tile image must register through the helper response",
    );
}

#[test]
fn empty_tile_caption_adds_no_label() {
    let mut harness = Harness::new();
    let texture = harness.ctx.load_texture(
        "tile-nocaption-tex",
        egui::ColorImage::new(
            [48, 48],
            vec![egui::Color32::from_rgb(45, 120, 200); 48 * 48],
        ),
        egui::TextureOptions::LINEAR,
    );
    let texture_id = texture.id();
    let mut body = |ui: &mut egui::Ui| {
        _ = functora_egui::Flex::row().gap(8.0).show(ui, |f| {
            _ = f.ui(|tile| {
                _ = functora_egui_demo::ShowcaseApp::image_tile(
                    tile,
                    160.0,
                    "",
                    egui::Image::from_texture(egui::load::SizedTexture::from_handle(&texture)),
                );
            });
        });
    };
    let out = harness.step(&mut body);
    let texts: Vec<String> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect();
    assert!(
        texts.is_empty(),
        "empty caption must not render any label, got {texts:?}",
    );
    assert_eq!(
        tile_image_centers(&out, texture_id).len(),
        1,
        "tile image must still render without a caption",
    );
}
