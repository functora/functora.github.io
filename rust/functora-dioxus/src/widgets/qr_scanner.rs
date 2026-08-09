#![allow(clippy::shadow_reuse)]
use crate::error::Error;
use crate::ffi::{capture_frame, check_camera, sleep, start_camera, stop_camera};
use crate::i18n::Language;
use crate::qr::decode_qr_rgba;
use dioxus::prelude::*;

const FPS_DELAY: u64 = 33;

fn camera_error(e: &Error) -> Error {
    match e {
        Error::JS(msg) => {
            let text = msg.clone();
            if text.contains("Permission") || text.contains("denied") || text.contains("NotAllowed") {
                Error::CameraPermissionDenied(text)
            } else {
                Error::CameraNotAvailable(text)
            }
        }
        other => Error::CameraNotAvailable(other.to_string()),
    }
}

fn report_camera_error(e: &Error, error: &mut Signal<Option<Error>>, event: Option<&EventHandler<Error>>) {
    let message = camera_error(e);
    error.set(Some(message));
    if let Some(callback) = event {
        callback.call(camera_error(e));
    }
}

#[component]
pub fn QrScanner(on_scan: EventHandler<String>, on_error: Option<EventHandler<Error>>, lang: Language) -> Element {
    let mut scanning = use_signal(|| true);
    let mut found = use_signal(|| false);
    let mut error = use_signal(|| Option::<Error>::None);

    _ = use_effect(move || {
        let _ = spawn(async move {
            if let Err(e) = check_camera().await {
                report_camera_error(&e, &mut error, on_error.as_ref());
                return;
            }
            if let Err(e) = start_camera().await {
                report_camera_error(&e, &mut error, on_error.as_ref());
                return;
            }
            _ = sleep(FPS_DELAY).await;
            while scanning() && !found() {
                if let Ok(frame) = capture_frame().await
                    && let Some(text) = decode_qr_rgba(&frame.data, frame.width, frame.height)
                {
                    found.set(true);
                    scanning.set(false);
                    on_scan.call(text);
                }
                _ = sleep(FPS_DELAY).await;
            }
            _ = stop_camera().await;
        });
    });

    use_drop(move || {
        scanning.set(false);
        let _ = spawn(async move {
            _ = stop_camera().await;
        });
    });

    rsx! {
        if error.read().is_none() {
            video { id: "qr-video", autoplay: true, playsinline: true }
            canvas { id: "qr-canvas", style: "display:none" }
        }
    }
}
