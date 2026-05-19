use crate::qr_decode::decode_qr_rgba;
use crate::*;

const FPS_DELAY: u64 = 33;

#[component]
pub fn QrScanner(on_scan: EventHandler<String>) -> Element {
    let mut message =
        use_signal(|| Option::<UiMessage>::None);
    let mut scanning = use_signal(|| true);
    let mut found = use_signal(|| false);

    use_effect(move || {
        spawn(async move {
            let cam_err = |e: EvalError| {
                let msg = e.to_string();
                if msg.contains("Permission")
                    || msg.contains("denied")
                    || msg.contains("NotAllowed")
                {
                    AppError::CameraPermissionDenied
                } else {
                    AppError::CameraNotAvailable
                }
            };
            if let Err(e) = js_check_camera().await {
                message.set(Some(UiMessage::Error(
                    cam_err(e),
                )));
                return;
            }
            if let Err(e) = js_start_camera().await {
                message.set(Some(UiMessage::Error(
                    cam_err(e),
                )));
                return;
            }
            js_sleep(FPS_DELAY).await.ok();
            while scanning() && !found() {
                if let Ok(frame) = js_capture_frame().await
                {
                    if let Some(text) = decode_qr_rgba(
                        &frame.data,
                        frame.width,
                        frame.height,
                    ) {
                        found.set(true);
                        scanning.set(false);
                        on_scan.call(text);
                    }
                }
                js_sleep(FPS_DELAY).await.ok();
            }
            js_stop_camera().await.ok();
        });
    });

    use_drop(move || {
        scanning.set(false);
        spawn(async move {
            js_stop_camera().await.ok();
        });
    });

    rsx! {
        section {
            if message.read().is_some() {
                Message { message }
            } else {
                video { id: "qr-video", autoplay: true, playsinline: true }
                canvas { id: "qr-canvas", style: "display:none" }
            }
        }
    }
}
