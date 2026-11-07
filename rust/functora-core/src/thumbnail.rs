#[cfg(not(target_arch = "wasm32"))]
use image::RgbImage;
#[cfg(not(target_arch = "wasm32"))]
use image::codecs::jpeg::JpegEncoder;
#[cfg(not(target_arch = "wasm32"))]
use image::imageops::thumbnail;
#[cfg(not(target_arch = "wasm32"))]
use std::io::Cursor;

#[cfg(not(target_arch = "wasm32"))]
const MAX_W: u32 = 360;
#[cfg(not(target_arch = "wasm32"))]
const MAX_H: u32 = 240;
#[cfg(not(target_arch = "wasm32"))]
const MAX_SAMPLES: u32 = 16;
#[cfg(not(target_arch = "wasm32"))]
const JPEG_QUALITY: u8 = 70;

#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn video_thumbnail(data: &[u8]) -> Option<Vec<u8>> {
    let mut src = Cursor::new(data);
    let mut mp4 = mp4::Mp4Reader::read_header(&mut src, data.len() as u64).ok()?;
    let (track_id, avcc, first, last) = {
        let (id, track) = mp4
            .tracks()
            .iter()
            .find(|(_, t)| t.track_type().is_ok_and(|tt| tt == mp4::TrackType::Video))?;
        let avcc = track.trak.mdia.minf.stbl.stsd.avc1.as_ref()?.avcc.clone();
        let first = track
            .trak
            .mdia
            .minf
            .stbl
            .stss
            .as_ref()
            .and_then(|s| s.entries.first().copied())
            .unwrap_or(1);
        let last = first
            .saturating_add(MAX_SAMPLES - 1)
            .min(track.sample_count());
        (*id, avcc, first, last)
    };
    let mut config = vec![
        avcc.configuration_version,
        avcc.avc_profile_indication,
        avcc.profile_compatibility,
        avcc.avc_level_indication,
        0xFC | avcc.length_size_minus_one,
        0xE0 | u8::try_from(avcc.sequence_parameter_sets.len()).ok()?,
    ];
    for sps in &avcc.sequence_parameter_sets {
        config.extend_from_slice(&u16::try_from(sps.bytes.len()).ok()?.to_be_bytes());
        config.extend_from_slice(&sps.bytes);
    }
    config.push(u8::try_from(avcc.picture_parameter_sets.len()).ok()?);
    for pps in &avcc.picture_parameter_sets {
        config.extend_from_slice(&u16::try_from(pps.bytes.len()).ok()?.to_be_bytes());
        config.extend_from_slice(&pps.bytes);
    }
    let avcc_config = rust_h264::nal::parse_avcc_config(&config).ok()?;
    let mut decoder = rust_h264::decoder::OrderedDecoder::new();
    for nal in avcc_config
        .sps_nals
        .iter()
        .chain(avcc_config.pps_nals.iter())
    {
        _ = decoder.decode_nal(nal).ok()?;
    }
    let mut frames = Vec::new();
    for sample_id in first..=last {
        let Some(sample) = mp4.read_sample(track_id, sample_id).ok()? else {
            continue;
        };
        for nal in rust_h264::nal::parse_avcc(&sample.bytes, avcc_config.length_size) {
            frames.extend(decoder.decode_nal(&nal).ok()?);
        }
        if !frames.is_empty() {
            break;
        }
    }
    if frames.is_empty() {
        frames.extend(decoder.flush());
    }
    let frame = frames.into_iter().next()?;
    if frame.width == 0 || frame.height == 0 {
        return None;
    }
    let rgb = RgbImage::from_raw(frame.width, frame.height, yuv_to_rgb(&frame)?)?;
    let (thumb_w, thumb_h) = fit(frame.width, frame.height, MAX_W, MAX_H);
    let thumb = thumbnail(&rgb, thumb_w, thumb_h);
    let mut jpeg = Vec::new();
    JpegEncoder::new_with_quality(&mut jpeg, JPEG_QUALITY)
        .encode(
            &thumb,
            thumb.width(),
            thumb.height(),
            image::ExtendedColorType::Rgb8,
        )
        .ok()?;
    Some(jpeg)
}

#[must_use]
pub fn jpeg_data_url(jpeg: Vec<u8>) -> String {
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD as BASE64;
    format!("data:image/jpeg;base64,{}", BASE64.encode(jpeg))
}

#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn planar_yuv_to_rgb(
    width: u32,
    height: u32,
    y_plane: &[u8],
    u_plane: &[u8],
    v_plane: &[u8],
) -> Option<Vec<u8>> {
    (width != 0 && height != 0).then_some(())?;
    let (width_px, height_px) = (usize::try_from(width).ok()?, usize::try_from(height).ok()?);
    ((width_px % 2 == 0) && (height_px % 2 == 0)).then_some(())?;
    let luma = width_px.checked_mul(height_px)?;
    (y_plane.len() >= luma).then_some(())?;
    let chroma = (width_px / 2).checked_mul(height_px / 2)?;
    (u_plane.len() >= chroma).then_some(())?;
    (v_plane.len() >= chroma).then_some(())?;
    let rgb_len = usize::try_from(u64::try_from(luma).ok()?.checked_mul(3)?).ok()?;
    let mut rgb = vec![0u8; rgb_len];
    for y_row in 0..height_px {
        for x_col in 0..width_px {
            let yy = i64::from(y_plane[y_row * width_px + x_col]) << 10;
            let uu = i64::from(u_plane[(y_row / 2) * (width_px / 2) + (x_col / 2)]) - 128;
            let vv = i64::from(v_plane[(y_row / 2) * (width_px / 2) + (x_col / 2)]) - 128;
            let red =
                u8::try_from(((yy + 1436 * vv + 0x200) >> 10).clamp(0, 255)).unwrap_or(u8::MAX);
            let green = u8::try_from(((yy - 352 * uu - 731 * vv + 0x200) >> 10).clamp(0, 255))
                .unwrap_or(u8::MAX);
            let blue =
                u8::try_from(((yy + 1814 * uu + 0x200) >> 10).clamp(0, 255)).unwrap_or(u8::MAX);
            let index = (y_row * width_px + x_col) * 3;
            rgb[index] = red;
            rgb[index + 1] = green;
            rgb[index + 2] = blue;
        }
    }
    Some(rgb)
}

#[cfg(not(target_arch = "wasm32"))]
fn yuv_to_rgb(frame: &rust_h264::decoder::Frame) -> Option<Vec<u8>> {
    planar_yuv_to_rgb(frame.width, frame.height, &frame.y, &frame.u, &frame.v)
}

use crate::files::FifoMemo;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{LazyLock, Mutex};

static MEMO: LazyLock<Mutex<FifoMemo<u64, Option<String>>>> =
    LazyLock::new(|| Mutex::new(FifoMemo::new()));

#[must_use]
pub fn cached_thumbnail(url: &str) -> Option<Option<String>> {
    let key = thumbnail_key(url);
    MEMO.lock().ok().and_then(|guard| guard.get(&key).cloned())
}

pub fn cache_thumbnail(url: &str, src: Option<String>) {
    remember(thumbnail_key(url), src);
}

fn thumbnail_key(url: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    url.hash(&mut hasher);
    hasher.finish()
}

fn remember(key: u64, src: Option<String>) {
    if let Ok(mut guard) = MEMO.lock() {
        _ = guard.insert(key, src);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn fit(width: u32, height: u32, max_w: u32, max_h: u32) -> (u32, u32) {
    if width <= max_w && height <= max_h {
        (width, height)
    } else if u64::from(max_w) * u64::from(height) <= u64::from(max_h) * u64::from(width) {
        let scaled_h =
            (u64::from(height) * u64::from(max_w) + u64::from(width) / 2) / u64::from(width);
        (
            max_w,
            u32::try_from(scaled_h.clamp(1, u64::from(max_h))).unwrap_or(1),
        )
    } else {
        let scaled_w =
            (u64::from(width) * u64::from(max_h) + u64::from(height) / 2) / u64::from(height);
        (
            u32::try_from(scaled_w.clamp(1, u64::from(max_w))).unwrap_or(1),
            max_h,
        )
    }
}
