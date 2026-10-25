//! File screen image preview: an image attachment opened from the home
//! list must paint a non-empty image rect on the File screen, not collapse
//! to zero height inside the shell column.

use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;

pub mod common;
use common::{UiDriver, all_texts};

const PIXEL_PNG: &[u8] = &[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 21, 196,
    137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68,
    174, 66, 96, 130,
];

fn image_rects(output: &egui::FullOutput) -> Vec<egui::Rect> {
    fn walk(shape: &egui::Shape, out: &mut Vec<egui::Rect>) {
        match shape {
            egui::Shape::Rect(rect_shape) => {
                if rect_shape.fill_texture_id() != egui::TextureId::default() {
                    out.push(rect_shape.rect);
                }
            }
            egui::Shape::Mesh(mesh) => {
                if mesh.texture_id != egui::TextureId::default() {
                    out.push(mesh.calc_bounds());
                }
            }
            egui::Shape::Vec(shapes) => {
                for inner in shapes {
                    walk(inner, out);
                }
            }
            _ => {}
        }
    }
    let mut rects = Vec::new();
    for clipped in &output.shapes {
        walk(&clipped.shape, &mut rects);
    }
    rects
}

fn find_row_view(output: &egui::FullOutput, name: &str) -> egui::Pos2 {
    let found = all_texts(output);
    let Some((_, row)) = found.iter().find(|(text, _)| text.contains(name)) else {
        panic!("attachment row must render; got {found:?}");
    };
    let row_y = row.center().y;
    let Some((_, view)) = found
        .iter()
        .filter(|(text, _)| text == "View")
        .min_by(|(_, a), (_, b)| {
            (a.center().y - row_y)
                .abs()
                .partial_cmp(&(b.center().y - row_y).abs())
                .unwrap_or(std::cmp::Ordering::Equal)
        })
    else {
        panic!("row View must render; got {found:?}");
    };
    view.center()
}

#[test]
fn image_attachment_shows_on_file_screen() {
    let mut app = CryptonoteApp::default();
    app.temporary.attachments.push(functora_egui::files::Attachment {
        name: "photo.png".to_owned(),
        data: PIXEL_PNG.into(),
    });
    let mut driver = UiDriver::with_fonts();
    for _ in 0..5 {
        let _ = driver.run(&mut app, vec![]);
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    let out = driver.run(&mut app, vec![]);
    driver.click(&mut app, find_row_view(&out, "photo.png"));
    assert_eq!(app.temporary.screen, Screen::File);
    let mut last_rects = Vec::new();
    for _ in 0..30 {
        let last = driver.run(&mut app, vec![]);
        last_rects = image_rects(&last);
        if last_rects.iter().any(|r| r.width() > 1.0 && r.height() > 1.0) {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    assert!(
        last_rects.iter().any(|r| r.width() > 1.0 && r.height() > 1.0),
        "file screen must paint a non-empty image rect, got {last_rects:?}"
    );
}
