use crate::js::{js_capture_frame, js_check_camera, js_sleep, js_start_camera, js_stop_camera};
use crate::qr::decode_qr_rgba;
use dioxus::prelude::*;

const FPS_DELAY: u64 = 33;

#[derive(Clone, Debug, PartialEq)]
pub enum QrMessage {
    CameraNotAvailable(String),
    CameraPermissionDenied(String),
}

impl QrMessage {
    pub fn text(&self) -> String {
        match self {
            QrMessage::CameraNotAvailable(msg) => format!("Camera not available: {msg}"),
            QrMessage::CameraPermissionDenied(msg) => format!("Camera permission denied: {msg}"),
        }
    }
}

#[component]
pub fn QrScanner(on_scan: EventHandler<String>, on_error: Option<EventHandler<QrMessage>>) -> Element {
    let mut scanning = use_signal(|| true);
    let mut found = use_signal(|| false);
    let mut error_msg = use_signal(String::new);

    #[allow(unused_must_use, unused_results)]
    use_effect(move || {
        spawn(async move {
            let cam_err = |e: dioxus::document::EvalError| {
                let msg = e.to_string();
                if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
                    QrMessage::CameraPermissionDenied(msg)
                } else {
                    QrMessage::CameraNotAvailable(msg)
                }
            };
            if let Err(e) = js_check_camera().await {
                let msg = cam_err(e);
                error_msg.set(msg.text());
                if let Some(callback) = &on_error {
                    callback.call(msg);
                }
                return;
            }
            if let Err(e) = js_start_camera().await {
                let msg = cam_err(e);
                error_msg.set(msg.text());
                if let Some(callback) = &on_error {
                    callback.call(msg);
                }
                return;
            }
            _ = js_sleep(FPS_DELAY).await;
            while scanning() && !found() {
                if let Ok(frame) = js_capture_frame().await
                    && let Some(text) = decode_qr_rgba(&frame.data, frame.width, frame.height)
                {
                    found.set(true);
                    scanning.set(false);
                    on_scan.call(text);
                }
                _ = js_sleep(FPS_DELAY).await;
            }
            _ = js_stop_camera().await;
        });
    });

    use_drop(move || {
        scanning.set(false);
        #[allow(unused_must_use, unused_results)]
        spawn(async move {
            _ = js_stop_camera().await;
        });
    });

    rsx! {
        section {
            if !error_msg.read().is_empty() {
                p { "txt": "r",
                    code { "{error_msg}" }
                }
            } else {
                video { id: "qr-video", autoplay: true, playsinline: true }
                canvas { id: "qr-canvas", style: "display:none" }
            }
        }
    }
}
