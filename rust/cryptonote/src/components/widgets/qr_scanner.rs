use crate::qr_decode::decode_qr_rgba;
use crate::*;

#[component]
pub fn QrScanner(on_scan: EventHandler<String>) -> Element {
    let t = get_translations(
        use_context::<Signal<AppCfg>>().read().language,
    );
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
            js_sleep(500).await.ok();
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
                js_sleep(200).await.ok();
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
            h3 { "{t.qr_scanner_title}" }
            video { id: "qr-video", autoplay: true, playsinline: true }
            canvas { id: "qr-canvas", style: "display:none" }
            if message.read().is_some() {
                Message { message }
            }
            p {
                Button {
                    icon: FaArrowLeft,
                    onclick: move |_| {
                        scanning.set(false);
                    },
                    "{t.back_button}"
                }
            }
        }
    }
}
