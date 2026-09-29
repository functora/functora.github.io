//! Preview URL truncation must never slice a multi-byte codepoint.

use functora_egui_demo::ShowcaseApp;

#[test]
fn ascii_url_truncates_at_byte_limit() {
    let url = format!("bytes://{}", "a".repeat(100));
    let shown = ShowcaseApp::truncate_preview_url(&url, 60);
    assert_eq!(shown.len(), 60);
    assert!(url.starts_with(shown));
}

#[test]
fn short_url_is_not_truncated() {
    let url = "bytes://short.mp4";
    assert_eq!(ShowcaseApp::truncate_preview_url(url, 60), url);
}

#[test]
fn multibyte_url_never_panics_at_boundary() {
    let mut url = "bytes://".to_owned();
    for _ in 0..40 {
        url.push('é');
    }
    for limit in 0..url.len() {
        let shown = ShowcaseApp::truncate_preview_url(&url, limit);
        assert!(url.starts_with(shown), "limit {limit} must cut a prefix");
    }
}

#[test]
fn emoji_data_url_never_panics_at_boundary() {
    let url = format!("data:video/mp4;base64,{}", "🎉🎉🎉🎉🎉🎉🎉🎉");
    for limit in 0..url.len() {
        let shown = ShowcaseApp::truncate_preview_url(&url, limit);
        assert!(url.starts_with(shown), "limit {limit} must cut a prefix");
        assert!(shown.len() <= limit);
    }
}

#[test]
fn empty_url_truncates_to_empty() {
    assert_eq!(ShowcaseApp::truncate_preview_url("", 60), "");
}
