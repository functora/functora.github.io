use functora_dioxus::thumbnail::cache_thumbnail;
use functora_dioxus::thumbnail::cached_thumbnail;
use functora_dioxus::thumbnail::jpeg_data_url;
#[cfg(not(target_arch = "wasm32"))]
use functora_dioxus::thumbnail::video_thumbnail;

#[cfg(not(target_arch = "wasm32"))]
fn thumb_of(video: &[u8]) -> image::DynamicImage {
    let jpeg = video_thumbnail(video).unwrap_or_else(|| panic!("fixture must yield a thumbnail"));
    assert_eq!(&jpeg[..2], &[0xFF, 0xD8]);
    image::load_from_memory(&jpeg).unwrap_or_else(|e| panic!("thumbnail must be a valid JPEG: {e}"))
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn video_thumbnail_extracts_first_frame_as_small_jpeg() {
    let video = include_bytes!("fixtures/tiny-h264.mp4");
    assert_eq!((thumb_of(video).width(), thumb_of(video).height()), (160, 120));
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn video_thumbnail_is_bounded_to_max_dimensions() {
    let video = include_bytes!("fixtures/tiny-h264.mp4");
    let img = thumb_of(video);
    assert!(img.width() <= 360);
    assert!(img.height() <= 240);
    assert_eq!(u64::from(img.width()) * 120, u64::from(img.height()) * 160);
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn video_thumbnail_rejects_garbage() {
    assert!(video_thumbnail(b"not an mp4").is_none());
    assert!(video_thumbnail(&[]).is_none());
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn video_thumbnail_is_deterministic() {
    let video = include_bytes!("fixtures/tiny-h264.mp4");
    let first = video_thumbnail(video).unwrap_or_else(|| panic!("fixture must yield a thumbnail"));
    let second = video_thumbnail(video).unwrap_or_else(|| panic!("fixture must yield a thumbnail again"));
    assert_eq!(first, second);
}

#[test]
fn jpeg_data_url_has_image_mime() {
    let url = jpeg_data_url(vec![0xFF, 0xD8, 0xFF]);
    assert!(url.starts_with("data:image/jpeg;base64,"));
}

#[test]
fn thumbnail_cache_memoizes_results() {
    let url = "data:video/mp4;base64,QUFBQQ";
    assert_eq!(cached_thumbnail(url), None);
    let src = "data:image/jpeg;base64,QkJC";
    cache_thumbnail(url, Some(src.to_string()));
    assert_eq!(cached_thumbnail(url), Some(Some(src.to_string())));
}

#[test]
fn thumbnail_cache_remembers_failures() {
    let url = "data:video/mp4;base64,Q0ND";
    assert_eq!(cached_thumbnail(url), None);
    cache_thumbnail(url, None);
    assert_eq!(cached_thumbnail(url), Some(None));
}

#[test]
fn thumbnail_cache_distinguishes_urls() {
    let first = "data:video/mp4;base64,QUFB";
    let second = "data:video/mp4;base64,QkJC";
    cache_thumbnail(first, Some("data:image/jpeg;base64,WA==".to_string()));
    assert_eq!(cached_thumbnail(second), None);
    assert_eq!(
        cached_thumbnail(first),
        Some(Some("data:image/jpeg;base64,WA==".to_string()))
    );
}
