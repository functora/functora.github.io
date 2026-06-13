use crate::messages::*;
use crate::*;

#[component]
pub fn Share() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let ctx = use_context::<Signal<AppCtx>>();
    let lang = use_lang();

    let mut url = use_signal(String::new);
    let mut qr_code = use_signal(String::new);
    let mut message = use_signal(|| Option::<Msg>::None);

    use_effect(move || {
        let res: Result<(String, String), AppError> =
            (|| {
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
                            let path = pathname
                                .trim_end_matches('/');
                            Some(format!(
                                "{}//{}{}",
                                protocol, host, path
                            ))
                        })
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        Some(WEB_APP_URL)
                    }
                }
                .ok_or(AppError::NoNoteInUrl)?;

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
                message
                    .set(Some(Msg::Error(e.render(lang))));
            }
        }
    });

    rsx! {
        Breadcrumb { title: Msg::ShareTitle }
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
                            write_clipboard(url(), message, Msg::Copied, |_e| Msg::ClipboardWriteError);
                        },
                    }

                    Dock { message,
                        Button {
                            icon: FaEye,
                            onclick: move |_| {
                                nav.write().push(Screen::View.to_route(None));
                            },
                            "{Msg::ViewButton.render(lang)}"
                        }
                        Button {
                            icon: FaCopy,
                            primary: true,
                            onclick: move |_| {
                                write_clipboard(url(), message, Msg::Copied, |_e| Msg::ClipboardWriteError);
                            },
                            "{Msg::CopyButton.render(lang)}"
                        }
                    }
                }
            } else if message.read().is_some() {
                Dock { message }
            } else {
                p { "{Msg::Loading.render(lang)}" }
            }
        }
    }
}
