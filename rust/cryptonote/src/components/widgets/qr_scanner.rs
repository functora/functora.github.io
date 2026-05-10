#[cfg(all(feature = "web", target_arch = "wasm32"))]
mod imp {
    use crate::qr_decode::decode_qr_rgba;
    use crate::*;
    use gloo_timers::future::TimeoutFuture;

    #[component]
    pub fn QrScanner(
        on_scan: EventHandler<String>,
        on_close: EventHandler<()>,
    ) -> Element {
        let cfg = use_context::<Signal<AppCfg>>();
        let t = get_translations(cfg.read().language);
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
                TimeoutFuture::new(500).await;
                while scanning() && !found() {
                    match js_capture_frame().await {
                        Ok(rgba) => {
                            let video =
                                crate::js::js_video_dimensions()
                                    .await
                                    .unwrap_or((1280, 720));
                            if let Some(text) =
                                decode_qr_rgba(
                                    &rgba, video.0, video.1,
                                )
                            {
                                found.set(true);
                                scanning.set(false);
                                on_scan.call(text);
                            }
                        }
                        Err(_) => {}
                    }
                    TimeoutFuture::new(33).await;
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
            div { class: "qr-scanner-overlay",
                div { class: "qr-scanner-container",
                    h3 { "{t.qr_scanner_title}" }
                    video {
                        id: "qr-video",
                        autoplay: true,
                        playsinline: true,
                        class: "qr-video",
                    }
                    canvas { id: "qr-canvas", style: "display:none" }
                    if message.read().is_some() {
                        Message { message }
                    }
                    div { class: "qr-scanner-actions",
                        Button {
                            icon: FaArrowLeft,
                            onclick: move |_| {
                                scanning.set(false);
                                on_close.call(());
                            },
                            "{t.back_button}"
                        }
                    }
                }
            }
        }
    }
}

#[cfg(not(all(feature = "web", target_arch = "wasm32")))]
mod imp {
    use crate::*;

    #[component]
    pub fn QrScanner(
        on_scan: EventHandler<String>,
        on_close: EventHandler<()>,
    ) -> Element {
        let cfg = use_context::<Signal<AppCfg>>();
        let t = get_translations(cfg.read().language);

        rsx! {
            div { class: "qr-scanner-overlay",
                div { class: "qr-scanner-container",
                    h3 { "{t.qr_scanner_title}" }
                    p { "{t.qr_camera_not_available}" }
                    div { class: "qr-scanner-actions",
                        Button {
                            icon: FaArrowLeft,
                            onclick: move |_| on_close.call(()),
                            "{t.back_button}"
                        }
                    }
                }
            }
        }
    }
}

pub use imp::QrScanner;
