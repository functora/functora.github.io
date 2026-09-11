//! Platform dispatch contracts (plan step 5a).
//!
//! These lock the observable behavior that the dispatch unification must
//! preserve: desktop camera ops report "use file picker", stop/sleep always
//! succeed, and share (clipboard fallback) always succeeds.

#![allow(clippy::unwrap_used, clippy::expect_used)]

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
mod desktop {
    fn camera_unavailable_message(error: &functora_egui::error::Error) -> String {
        match error {
            functora_egui::error::Error::CameraNotAvailable(message) => message.clone(),
            other => panic!("expected CameraNotAvailable, got {other:?}"),
        }
    }

    #[test]
    fn check_camera_reports_file_picker_fallback() {
        let error = pollster::block_on(functora_egui::camera::check_camera()).unwrap_err();
        assert!(
            camera_unavailable_message(&error).contains("file picker"),
            "unexpected message: {error:?}"
        );
    }

    #[test]
    fn start_camera_reports_file_picker_fallback() {
        let error = pollster::block_on(functora_egui::camera::start_camera()).unwrap_err();
        assert!(
            camera_unavailable_message(&error).contains("file picker"),
            "unexpected message: {error:?}"
        );
    }

    #[test]
    fn capture_frame_reports_file_picker_fallback() {
        let error = pollster::block_on(functora_egui::camera::capture_frame()).unwrap_err();
        assert!(
            camera_unavailable_message(&error).contains("file picker"),
            "unexpected message: {error:?}"
        );
    }

    #[test]
    fn stop_camera_always_succeeds() {
        pollster::block_on(functora_egui::camera::stop_camera()).unwrap();
    }

    #[test]
    fn sleep_always_succeeds() {
        pollster::block_on(functora_egui::camera::sleep(1)).unwrap();
    }

    #[test]
    fn capture_session_helpers_do_not_panic() {
        functora_egui::camera::begin_capture_session();
        functora_egui::camera::stop_capture_worker();
    }

    #[test]
    fn share_falls_back_to_clipboard_and_succeeds() {
        let data = functora_egui::share::ShareData {
            title: "title".into(),
            text: "text".into(),
            url: "https://example.com".into(),
        };
        pollster::block_on(functora_egui::share::share(data)).unwrap();
    }
}
