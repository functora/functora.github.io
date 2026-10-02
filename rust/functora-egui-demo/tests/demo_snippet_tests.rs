//! Regression tests for demo snippet accuracy (plan steps 2-3).
//!
//! Snippet strings duplicate live code and drift. The command palette snippet
//! must use the same `CommandValue` + `CommandItem` API as the live
//! overlay in `app.rs`. The thumbnail/zip/crypto snippets must show the real
//! wiring used by the live demos, not aspirational lib calls with wrong
//! signatures.

const OVERLAY_SRC: &str = include_str!("../src/screens/overlay/command.rs");
const APP_SRC: &str = include_str!("../src/app.rs");
const PLATFORM_THUMBNAIL_SRC: &str = include_str!("../src/screens/platform/thumbnail.rs");
const PLATFORM_ZIP_SRC: &str = include_str!("../src/screens/platform/zip.rs");
const PLATFORM_CRYPTO_SRC: &str = include_str!("../src/screens/platform/crypto.rs");
const PLATFORM_COMMON_SRC: &str = include_str!("../src/screens/platform/common.rs");

#[test]
fn live_overlay_uses_value_api() {
    assert!(
        APP_SRC.contains("CommandValue::new"),
        "live app.rs must build the palette with CommandValue::new"
    );
}

#[test]
fn command_snippet_matches_live_value_api() {
    assert!(
        OVERLAY_SRC.contains("CommandValue::new"),
        "demo_command snippet must use CommandValue::new like live app.rs"
    );
    assert!(
        OVERLAY_SRC.contains("CommandItem"),
        "demo_command snippet must mention CommandItem like live app.rs"
    );
}

#[test]
fn thumbnail_snippet_shows_real_files_api() {
    assert!(
        PLATFORM_THUMBNAIL_SRC.contains("files::video_thumbnail")
            || PLATFORM_COMMON_SRC.contains("files::video_thumbnail"),
        "demo_thumbnail snippet must show the real files::video_thumbnail call"
    );
    assert!(
        PLATFORM_THUMBNAIL_SRC.contains("from_bytes") || PLATFORM_COMMON_SRC.contains("from_bytes"),
        "demo_thumbnail snippet must show from_bytes display like the live demo"
    );
}

#[test]
fn zip_snippet_shows_real_roundtrip() {
    assert!(
        PLATFORM_ZIP_SRC.contains("verify_zip_roundtrip")
            || PLATFORM_COMMON_SRC.contains("verify_zip_roundtrip"),
        "demo_zip snippet must show verify_zip_roundtrip like the live demo"
    );
}

#[test]
fn crypto_snippet_shows_real_helpers() {
    assert!(
        PLATFORM_CRYPTO_SRC.contains("encrypt_output")
            || PLATFORM_COMMON_SRC.contains("encrypt_output"),
        "demo_crypto snippet must show encrypt_output like the live demo"
    );
    assert!(
        PLATFORM_CRYPTO_SRC.contains("decrypt_output")
            || PLATFORM_COMMON_SRC.contains("decrypt_output"),
        "demo_crypto snippet must show decrypt_output like the live demo"
    );
}
