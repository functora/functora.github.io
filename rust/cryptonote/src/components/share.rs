use crate::*;

#[component]
pub fn Share() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let ctx = use_context::<Signal<AppCtx>>();
    let t = get_translations(cfg.read().language);

    let mut url = use_signal(String::new);
    let mut qr_code = use_signal(String::new);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);

    use_effect(move || {
        let res = (|| {
            let ctx = ctx.read();

            let note_data = ctx.cipher.map_or_else(
                || {
                    Ok(NoteData::PlainText(
                        ctx.content.clone(),
                    ))
                },
                |cipher| {
                    if ctx.password.is_empty() {
                        Err(AppError::PasswordRequired)
                    } else {
                        encrypt_symmetric(
                            ctx.content.as_bytes(),
                            &ctx.password,
                            cipher,
                        )
                        .map(NoteData::CipherText)
                    }
                },
            )?;

            let origin = {
                #[cfg(target_arch = "wasm32")]
                {
                    web_sys::window().and_then(|w| {
                        let loc = w.location();
                        let protocol =
                            loc.protocol().ok()?;
                        let host = loc.host().ok()?;
                        let pathname =
                            loc.pathname().ok()?;
                        let path =
                            pathname.trim_end_matches('/');
                        Some(format!(
                            "{}//{}{}",
                            protocol, host, path
                        ))
                    })
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    Some(format!(
                        "https://functora.github.io/apps/cryptonote/{}",
                        env!("CARGO_PKG_VERSION")
                    ))
                }
            }.ok_or(AppError::NoNoteInUrl)?;

            let view_url = format!(
                "{}/?screen={}",
                origin,
                Screen::View
            );
            let url = build_url(&view_url, &note_data)?;
            let qr = generate_qr_code(&url)?;

            Ok((url, qr))
        })();

        match res {
            Ok((u, q)) => {
                url.set(u);
                qr_code.set(q);
                message.set(None);
            }
            Err(e) => {
                message.set(Some(UiMessage::Error(e)));
            }
        }
    });

    rsx! {
        Breadcrumb { title: t.share_title.to_string() }
        section {

            if !url().is_empty() {
                fieldset {
                    if !qr_code().is_empty() {
                        div { dangerous_inner_html: "{qr_code}" }
                    }

                    textarea {
                        readonly: true,
                        value: "{url}",
                        onclick: move |_| {
                            let url_val = url();
                            spawn(async move {
                                match js_write_clipboard(url_val).await {
                                    Ok(()) => message.set(Some(UiMessage::Copied)),
                                    Err(e) => {
                                        message.set(Some(UiMessage::Error(AppError::JsWriteClipboard(e))))
                                    }
                                }
                            });
                        },
                    }

                    Dock { message,
                        Button {
                            icon: FaCopy,
                            primary: true,
                            onclick: move |_| {
                                let url_val = url();
                                spawn(async move {
                                    match js_write_clipboard(url_val).await {
                                        Ok(()) => message.set(Some(UiMessage::Copied)),
                                        Err(e) => {
                                            message.set(Some(UiMessage::Error(AppError::JsWriteClipboard(e))))
                                        }
                                    }
                                });
                            },
                            "{t.copy_button}"
                        }
                    }
                }
            } else if !message.read().is_none() {
                Dock { message }
            } else {
                p { "{t.loading}" }
            }
        }
    }
}
