//! Camera frame conversion: rotation, NV21 decoding and preview sizing on
//! raw byte buffers, independent of any camera platform backend.

/// Total `u32` pixel-area product. Uses `u64` math so hostile dimensions
/// saturate instead of overflowing (which would panic in debug builds).
#[must_use]
pub fn pixel_area_len(width: u32, height: u32) -> usize {
    usize::try_from(u64::from(width) * u64::from(height)).unwrap_or(usize::MAX)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FrameRotation {
    Zero,
    Cw90,
    Cw180,
    Cw270,
}

impl FrameRotation {
    #[must_use]
    pub fn from_degrees_cw(degrees: u32) -> Self {
        if degrees == 90 {
            Self::Cw90
        } else if degrees == 180 {
            Self::Cw180
        } else if degrees == 270 {
            Self::Cw270
        } else {
            Self::Zero
        }
    }
}

const MAX_FRAME_AREA: usize = 16_777_216;

#[must_use]
pub fn rotated_dimensions(width: u32, height: u32, rotation: FrameRotation) -> (u32, u32) {
    match rotation {
        FrameRotation::Zero | FrameRotation::Cw180 => (width, height),
        FrameRotation::Cw90 | FrameRotation::Cw270 => (height, width),
    }
}

#[must_use]
pub fn rotate_luma(
    luma: &[u8],
    width: u32,
    height: u32,
    rotation: FrameRotation,
) -> (Vec<u8>, u32, u32) {
    let (out_w, out_h) = rotated_dimensions(width, height, rotation);
    let area = pixel_area_len(width, height);
    if width == 0 || height == 0 || area > MAX_FRAME_AREA {
        (Vec::new(), out_w, out_h)
    } else {
        let src_w = width as usize;
        let src_h = height as usize;
        let dst_w = out_w as usize;
        let dst_h = out_h as usize;
        let pixels: Vec<u8> = (0..dst_h)
            .flat_map(|dst_y| {
                (0..dst_w).map(move |dst_x| {
                    let (src_x, src_y) = match rotation {
                        FrameRotation::Zero => (dst_x, dst_y),
                        FrameRotation::Cw90 => {
                            (dst_y, src_h.saturating_sub(1).saturating_sub(dst_x))
                        }
                        FrameRotation::Cw180 => (
                            src_w.saturating_sub(1).saturating_sub(dst_x),
                            src_h.saturating_sub(1).saturating_sub(dst_y),
                        ),
                        FrameRotation::Cw270 => {
                            (src_w.saturating_sub(1).saturating_sub(dst_y), dst_x)
                        }
                    };
                    luma.get(src_y.saturating_mul(src_w).saturating_add(src_x))
                        .copied()
                        .unwrap_or(0)
                })
            })
            .collect();
        (pixels, out_w, out_h)
    }
}

#[must_use]
pub fn rotate_rgba(
    rgba: &[u8],
    width: u32,
    height: u32,
    rotation: FrameRotation,
) -> (Vec<u8>, u32, u32) {
    let (out_w, out_h) = rotated_dimensions(width, height, rotation);
    let area = pixel_area_len(width, height);
    if width == 0
        || height == 0
        || area > MAX_FRAME_AREA
        || area.saturating_mul(4) > MAX_FRAME_AREA.saturating_mul(4)
    {
        (Vec::new(), out_w, out_h)
    } else {
        let src_w = width as usize;
        let src_h = height as usize;
        let dst_w = out_w as usize;
        let dst_h = out_h as usize;
        let pixels: Vec<u8> = (0..dst_h)
            .flat_map(|dst_y| {
                (0..dst_w).flat_map(move |dst_x| {
                    let (src_x, src_y) = match rotation {
                        FrameRotation::Zero => (dst_x, dst_y),
                        FrameRotation::Cw90 => {
                            (dst_y, src_h.saturating_sub(1).saturating_sub(dst_x))
                        }
                        FrameRotation::Cw180 => (
                            src_w.saturating_sub(1).saturating_sub(dst_x),
                            src_h.saturating_sub(1).saturating_sub(dst_y),
                        ),
                        FrameRotation::Cw270 => {
                            (src_w.saturating_sub(1).saturating_sub(dst_y), dst_x)
                        }
                    };
                    let base = src_y
                        .saturating_mul(src_w)
                        .saturating_add(src_x)
                        .saturating_mul(4);
                    (0..4).map(move |channel| {
                        rgba.get(base.saturating_add(channel))
                            .copied()
                            .unwrap_or(if channel == 3 { 255 } else { 0 })
                    })
                })
            })
            .collect();
        (pixels, out_w, out_h)
    }
}

pub const RESPONSIVE_PREVIEW_MAX_SIDE: f32 = 480.0;
pub const RESPONSIVE_PREVIEW_FALLBACK_WIDTH: f32 = 320.0;
pub const RESPONSIVE_PREVIEW_FALLBACK_ASPECT: f32 = 4.0 / 3.0;

#[must_use]
pub fn responsive_preview_size(
    available_w: f32,
    available_h: f32,
    viewport_w: f32,
    viewport_h: f32,
    frame_width: u32,
    frame_height: u32,
) -> egui::Vec2 {
    let width = (if available_w.is_finite() && available_w > 0.0 {
        available_w
    } else {
        RESPONSIVE_PREVIEW_FALLBACK_WIDTH
    })
    .clamp(1.0, RESPONSIVE_PREVIEW_MAX_SIDE);
    let aspect = if frame_width == 0 || frame_height == 0 {
        RESPONSIVE_PREVIEW_FALLBACK_ASPECT
    } else {
        crate::cast::u32_to_f32(frame_width) / crate::cast::u32_to_f32(frame_height).max(1.0)
    }
    .max(0.01);
    let driven_h = (width / aspect).max(1.0);
    let portrait = viewport_w.is_finite()
        && viewport_h.is_finite()
        && viewport_w > 0.0
        && viewport_h > viewport_w;
    if portrait || !(available_h.is_finite() && available_h > 0.0) {
        egui::vec2(width, driven_h)
    } else {
        let height_cap = available_h.min(RESPONSIVE_PREVIEW_MAX_SIDE);
        let capped_h = driven_h.min(height_cap.max(1.0));
        egui::vec2((capped_h * aspect).max(1.0), capped_h)
    }
}

#[must_use]
pub fn fit_preview_size(desired: egui::Vec2, frame_width: u32, frame_height: u32) -> egui::Vec2 {
    if frame_width == 0 || frame_height == 0 || desired.x <= 0.0 || desired.y <= 0.0 {
        desired
    } else {
        let frame_aspect =
            crate::cast::u32_to_f32(frame_width) / crate::cast::u32_to_f32(frame_height).max(1.0);
        let box_aspect = desired.x / desired.y.max(1.0);
        if frame_aspect > box_aspect {
            egui::vec2(desired.x, (desired.x / frame_aspect.max(0.01)).max(1.0))
        } else {
            egui::vec2((desired.y * frame_aspect).max(1.0), desired.y)
        }
    }
}

#[must_use]
pub fn nv21_luma(nv21: &[u8], width: u32, height: u32) -> Vec<u8> {
    let len = pixel_area_len(width, height);
    nv21.get(..len).map_or_else(Vec::new, <[u8]>::to_vec)
}

/// BT.601 integer NV21 -> RGBA8888 conversion.
#[must_use]
pub fn nv21_to_rgba(nv21: &[u8], width: u32, height: u32) -> Vec<u8> {
    let cols = width.max(1);
    let rows = height.max(1);
    let y_len = pixel_area_len(cols, rows);
    let clamp = |value: i32| u8::try_from(value.clamp(0, 255)).unwrap_or(u8::MAX);
    let uv_offset = |row: usize, plane: usize, stride: u32| plane + (row / 2) * stride as usize;
    (0..rows as usize)
        .flat_map(|row| {
            let row_uv = uv_offset(row, y_len, cols);
            (0..cols as usize).map(move |col| {
                let y_index = row * cols as usize + col;
                let uv_index = row_uv + (col / 2) * 2;
                let luma = i32::from(nv21.get(y_index).copied().unwrap_or(235)).saturating_sub(16);
                let cr = i32::from(nv21.get(uv_index).copied().unwrap_or(128)) - 128;
                let cb = i32::from(nv21.get(uv_index + 1).copied().unwrap_or(128)) - 128;
                (
                    clamp((298 * luma + 402 * cr + 128) >> 8),
                    clamp((298 * luma - 100 * cb - 208 * cr + 128) >> 8),
                    clamp((298 * luma + 516 * cb + 128) >> 8),
                    255_u8,
                )
            })
        })
        .flat_map(|(red, green, blue, alpha)| [red, green, blue, alpha])
        .collect()
}

/// Converts a frames-per-second rate into a millisecond capture interval,
/// clamped to the 1..=60 fps range with a 16 ms floor.
#[must_use]
pub fn fps_to_interval_ms(fps: f32) -> u64 {
    use num_traits::ToPrimitive as _;
    let clamped = fps.clamp(1.0, 60.0);
    let millis = (1000.0 / clamped).round().max(16.0);
    millis.to_u64().unwrap_or(66)
}
