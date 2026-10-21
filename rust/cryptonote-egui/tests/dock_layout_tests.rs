//! Dock layout: dock buttons must render as one horizontal button row
//! (`Flex` + `dock_back_flex`), not stacked vertically. Screens that add
//! dock actions via `ui.add` after `dock_back` split Back from its sibling
//! buttons into separate rows.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;
use functora_egui::i18n::I18N;

const PIXEL_PNG: &[u8] = &[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 21, 196,
    137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68,
    174, 66, 96, 130,
];

struct UiDriver {
    ctx: egui::Context,
    frame: u32,
}

impl UiDriver {
    fn new() -> Self {
        let ctx = egui::Context::default();
        functora_egui::setup_fonts(&ctx);
        functora_egui::setup_image_loaders(&ctx);
        Self { ctx, frame: 0 }
    }

    fn run(&mut self, app: &mut CryptonoteApp, events: Vec<egui::Event>) -> egui::FullOutput {
        self.frame += 1;
        let raw = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::Vec2::new(1280.0, 800.0),
            )),
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

    fn click(&mut self, app: &mut CryptonoteApp, pos: egui::Pos2) {
        let press = |pressed: bool| egui::Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed,
            modifiers: egui::Modifiers::default(),
        };
        let _ = self.run(app, vec![egui::Event::PointerMoved(pos)]);
        let _ = self.run(app, vec![press(true)]);
        let _ = self.run(app, vec![press(false)]);
        let _ = self.run(app, vec![]);
    }
}

fn all_texts(output: &egui::FullOutput) -> Vec<(String, egui::Rect)> {
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

fn lang() -> functora_egui::i18n::Language {
    cryptonote_egui::detect_browser_language()
}

fn find_text(output: &egui::FullOutput, label: &str) -> egui::Rect {
    all_texts(output)
        .iter()
        .find_map(|(text, rect)| if text == label { Some(*rect) } else { None })
        .unwrap_or_else(|| panic!("{label:?} must render; got {:?}", all_texts(output)))
}

fn assert_same_row(a: egui::Rect, b: egui::Rect, what: &str) {
    let dy = (a.center().y - b.center().y).abs();
    assert!(
        dy < 8.0,
        "{what}: buttons must share one dock row, got dy={dy} between {a:?} and {b:?}"
    );
}

fn center_of_rect_at(output: &egui::FullOutput, pred: impl Fn(&str) -> bool) -> egui::Pos2 {
    all_texts(output).iter().find(|(text, _)| pred(text)).map_or_else(
        || panic!("expected text must render; got {:?}", all_texts(output)),
        |(_, rect)| rect.center(),
    )
}

#[test]
fn file_screen_dock_buttons_share_one_row() {
    let mut app = CryptonoteApp::default();
    app.temporary.attachments.push(functora_egui::files::Attachment {
        name: "photo.png".to_owned(),
        data: PIXEL_PNG.into(),
    });
    let mut driver = UiDriver::new();
    let out = driver.run(&mut app, vec![]);
    let row = center_of_rect_at(&out, |text| text.contains("photo.png"));
    let texts = all_texts(&out);
    let view = texts
        .iter()
        .filter(|(text, _)| text == "View")
        .min_by(|(_, a), (_, b)| {
            (a.center().y - row.y)
                .abs()
                .partial_cmp(&(b.center().y - row.y).abs())
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .map_or_else(|| panic!("row View must render"), |(_, rect)| rect.center());
    driver.click(&mut app, view);
    let _ = driver.run(&mut app, vec![]);
    assert_eq!(app.temporary.screen, Screen::File);
    let file_out = driver.run(&mut app, vec![]);
    let back = find_text(&file_out, &functora_egui::messages::Msg::Back.render(lang()));
    let save = find_text(&file_out, &cryptonote_egui::Msg::Save.render(lang()));
    assert_same_row(back, save, "file screen dock");
}

#[test]
fn view_screen_save_all_is_in_dock_row() {
    let mut app = CryptonoteApp::default();
    app.temporary.attachments.push(functora_egui::files::Attachment {
        name: "photo.png".to_owned(),
        data: PIXEL_PNG.into(),
    });
    let mut driver = UiDriver::new();
    let out = driver.run(&mut app, vec![]);
    let dock_view = all_texts(&out)
        .iter()
        .filter(|(text, _)| text == "View")
        .max_by(|(_, a), (_, b)| {
            a.center()
                .y
                .partial_cmp(&b.center().y)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .map_or_else(|| panic!("dock View must render"), |(_, rect)| rect.center());
    driver.click(&mut app, dock_view);
    let _ = driver.run(&mut app, vec![]);
    assert_eq!(app.temporary.screen, Screen::View);
    let view_out = driver.run(&mut app, vec![]);
    let back = find_text(&view_out, &functora_egui::messages::Msg::Back.render(lang()));
    let save_all = find_text(&view_out, &cryptonote_egui::Msg::SaveAll.render(lang()));
    assert_same_row(back, save_all, "view screen dock");
}
