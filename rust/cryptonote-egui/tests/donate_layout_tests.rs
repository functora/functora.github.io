#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

struct UiDriver {
    ctx: egui::Context,
    frame: u32,
}

impl UiDriver {
    fn new() -> Self {
        Self {
            ctx: egui::Context::default(),
            frame: 0,
        }
    }

    fn run(&mut self, app: &mut CryptonoteApp, size: egui::Vec2, events: Vec<egui::Event>) -> egui::FullOutput {
        self.frame += 1;
        let raw = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(egui::Pos2::ZERO, size)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut frame = eframe::Frame::_new_kittest();
        let mut out = self.ctx.run_ui(raw, |ui| {
            eframe::App::ui(app, ui, &mut frame);
        });
        out.textures_delta.clear();
        out
    }

    fn click(&mut self, app: &mut CryptonoteApp, size: egui::Vec2, pos: egui::Pos2) {
        let press = |pressed: bool| egui::Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed,
            modifiers: egui::Modifiers::default(),
        };
        let _ = self.run(app, size, vec![egui::Event::PointerMoved(pos)]);
        let _ = self.run(app, size, vec![press(true)]);
        let _ = self.run(app, size, vec![press(false)]);
        let _ = self.run(app, size, vec![]);
    }
}

fn collect_texts(shape: &egui::Shape, out: &mut Vec<(String, egui::Rect)>) {
    match shape {
        egui::Shape::Text(text) => out.push((
            text.galley.text().to_owned(),
            text.galley.rect.translate(text.pos.to_vec2()),
        )),
        egui::Shape::Vec(shapes) => {
            for inner in shapes {
                collect_texts(inner, out);
            }
        }
        _ => {}
    }
}

fn collect_rects(output: &egui::FullOutput) -> Vec<egui::epaint::RectShape> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect_shape) => Some(rect_shape.clone()),
            _ => None,
        })
        .collect()
}

fn is_plain(rect_shape: &egui::epaint::RectShape, color: egui::Color32) -> bool {
    rect_shape.fill == color
        && rect_shape
            .brush
            .as_ref()
            .is_none_or(|brush| brush.fill_texture_id == egui::TextureId::default())
}

fn topmost(rects: &[egui::Rect]) -> egui::Rect {
    rects
        .iter()
        .min_by(|a, b| a.min.y.total_cmp(&b.min.y))
        .copied()
        .expect("rect must render")
}

const DESKTOP: egui::Vec2 = egui::Vec2::new(1280.0, 800.0);

#[test]
fn donate_blocks_span_full_width_on_any_screen() {
    let mut app = CryptonoteApp::default();
    app.persistent.theme = functora_egui::theme_extra::Theme::Dark;
    let mut driver = UiDriver::new();
    let lang = cryptonote_egui::detect_browser_language();
    let out = driver.run(&mut app, DESKTOP, vec![]);
    let mut texts = Vec::new();
    for clipped in &out.shapes {
        collect_texts(&clipped.shape, &mut texts);
    }
    let donate = BaseMsg::Donate.render(lang);
    let pos = texts
        .iter()
        .find(|(text, _)| text == &donate)
        .map_or_else(|| panic!("Donate must render"), |(_, rect)| rect.center());
    driver.click(&mut app, DESKTOP, pos);
    let _ = driver.run(&mut app, DESKTOP, vec![]);
    assert_eq!(app.temporary.screen, Screen::Donate);
    for size in [
        egui::Vec2::new(390.0, 844.0),
        egui::Vec2::new(768.0, 1024.0),
        DESKTOP,
        egui::Vec2::new(1920.0, 1080.0),
    ] {
        let sized = driver.run(&mut app, size, vec![]);
        assert_eq!(app.temporary.screen, Screen::Donate);
        let rects = collect_rects(&sized);
        let images: Vec<egui::Rect> = rects
            .iter()
            .filter(|rect_shape| {
                rect_shape
                    .brush
                    .as_ref()
                    .is_some_and(|brush| brush.fill_texture_id != egui::TextureId::default())
            })
            .map(|rect_shape| rect_shape.rect)
            .collect();
        let side = images.iter().map(egui::Rect::width).fold(0.0, f32::max);
        assert!(side > 0.0, "QR image must render at {size:?}");
        let whites: Vec<egui::Rect> = rects
            .iter()
            .filter(|rect_shape| is_plain(rect_shape, egui::Color32::WHITE) && rect_shape.rect.height() > 100.0)
            .map(|rect_shape| rect_shape.rect)
            .collect();
        assert!(!whites.is_empty(), "white quiet-zone box must render at {size:?}");
        let white = topmost(&whites);
        let fields: Vec<egui::Rect> = rects
            .iter()
            .filter(|rect_shape| (rect_shape.rect.height() - 80.0).abs() <= 1.0 && rect_shape.rect.width() > 100.0)
            .map(|rect_shape| rect_shape.rect)
            .collect();
        assert!(!fields.is_empty(), "address textarea must render at {size:?}");
        let field = topmost(&fields);
        assert!(
            (white.width() - (side + 24.0)).abs() <= 3.0,
            "white box must hug the QR square at {size:?}, got {white:?} for side {side:.1}"
        );
        assert!(
            white.width() <= size.x + 1.0,
            "white box must not overflow at {size:?}, got {white:?}"
        );
        assert!(
            field.width() <= size.x + 1.0,
            "address field must not overflow at {size:?}, got {field:?}"
        );
        if size.x >= 1280.0 {
            assert!(
                field.width() >= 900.0,
                "address field must span full width like Share at {size:?}, got {field:?}"
            );
        }
        assert!(
            (white.center().x - field.center().x).abs() <= 4.0,
            "QR box and address field must share one column at {size:?}, got {white:?} vs {field:?}"
        );
    }
}
