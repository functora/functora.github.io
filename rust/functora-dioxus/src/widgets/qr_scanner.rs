use crate::error::Error;
use crate::ffi::{capture_frame, check_camera, sleep, start_camera, stop_camera};
use crate::i18n::Language;
use crate::qr::decode_qr_rgba;
use dioxus::prelude::*;

const FPS_DELAY: u64 = 33;

fn cam_err(e: &Error) -> Error {
    match e {
        Error::JS(eval_err) => {
            let msg = eval_err.to_string();
            if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
                Error::CameraPermissionDenied(msg)
            } else {
                Error::CameraNotAvailable(msg)
            }
        }
        other => Error::CameraNotAvailable(other.to_string()),
    }
}

#[component]
pub fn QrScanner(on_scan: EventHandler<String>, on_error: Option<EventHandler<Error>>, lang: Language) -> Element {
    let mut scanning = use_signal(|| true);
    let mut found = use_signal(|| false);
    let mut error = use_signal(|| Option::<Error>::None);

    #[allow(unused_must_use, unused_results)]
    use_effect(move || {
        spawn(async move {
            if let Err(e) = check_camera().await {
                let msg = cam_err(&e);
                error.set(Some(msg.clone()));
                if let Some(callback) = &on_error {
                    callback.call(msg);
                }
                return;
            }
            if let Err(e) = start_camera().await {
                let msg = cam_err(&e);
                error.set(Some(msg.clone()));
                if let Some(callback) = &on_error {
                    callback.call(msg);
                }
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
        #[allow(unused_must_use, unused_results)]
        spawn(async move {
            _ = stop_camera().await;
        });
    });

    rsx! {
        section {
            crate::widgets::Banner { message: error, lang }
            if error.read().is_none() {
                video { id: "qr-video", autoplay: true, playsinline: true }
                canvas { id: "qr-canvas", style: "display:none" }
            }
        }
    }
}
