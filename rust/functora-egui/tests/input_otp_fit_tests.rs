//! `InputOtp`: the digit strip is one row of boxes that must fit the room it
//! is given, shrinking its cells on narrow screens instead of pushing the
//! last boxes off the screen.

pub mod common;
use common::Harness;

use egui::{Rect, Vec2};

fn otp_response_rect(screen: Vec2, length: usize) -> Rect {
    let mut value = String::new();
    let mut rect = Rect::NOTHING;
    let mut app = Harness::new(screen);
    let _ = app.step(&mut |ui| {
        rect = functora_egui::InputOtp::new(length)
            .show(ui, &mut value)
            .rect;
    });
    rect
}

#[test]
fn otp_strip_fits_narrow_screens() {
    for (width, length) in [
        (240.0_f32, 6_usize),
        (240.0, 8),
        (320.0, 8),
        (360.0, 6),
        (1280.0, 6),
    ] {
        let rect = otp_response_rect(Vec2::new(width, 640.0), length);
        assert!(
            rect.max.x <= width,
            "an OTP of {length} boxes at {width}px spills to {rect:?}"
        );
    }
}

#[test]
fn otp_cells_keep_full_size_when_there_is_room() {
    let rect = otp_response_rect(Vec2::new(1280.0, 640.0), 6);
    assert!(
        (rect.width() - (40.0 * 6.0 + 8.0 * 5.0)).abs() < 1.0,
        "6 cells with room to spare must keep their 40px size, got {rect:?}"
    );
}
