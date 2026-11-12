#![cfg(feature = "files")]
#![cfg(not(target_arch = "wasm32"))]

use functora_egui::files::{PickerBackend, desktop_display_available, picker_backend};

#[test]
fn the_portal_is_preferred_when_the_session_has_one() {
    assert_eq!(picker_backend(true, true), PickerBackend::Portal);
    assert_eq!(picker_backend(true, false), PickerBackend::Portal);
}

#[test]
fn the_platform_dialog_is_the_fallback_without_a_portal() {
    assert_eq!(picker_backend(false, true), PickerBackend::Native);
}

#[test]
fn no_backend_can_run_without_a_portal_and_without_a_display() {
    assert_eq!(picker_backend(false, false), PickerBackend::Unavailable);
}

#[test]
fn the_display_probe_follows_the_environment() {
    let announced =
        std::env::var_os("WAYLAND_DISPLAY").is_some() || std::env::var_os("DISPLAY").is_some();
    assert_eq!(desktop_display_available(), announced);
}

#[test]
#[cfg(feature = "runtime")]
fn the_picker_reports_a_failure_instead_of_hanging_without_a_backend() {
    use std::time::Duration;

    if desktop_display_available() {
        return;
    }
    let receiver = functora_egui::spawn_async(functora_egui::files::pick_files(true));
    let outcome = receiver.recv_timeout(Duration::from_secs(60));
    assert!(
        outcome.is_ok(),
        "the picker must not hang when no backend can show a dialog"
    );
    let Ok(picked) = outcome else { return };
    assert!(
        picked.is_err(),
        "a picker without a backend must not report attached files"
    );
}
