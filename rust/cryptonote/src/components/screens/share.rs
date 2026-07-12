use crate::messages::*;
use crate::*;

#[component]
pub fn Share() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();

    let mut url = use_signal(String::new);
    let mut qr_code = use_signal(String::new);
    let mut message = use_message();

    use_effect(move || {
        let content = tst.content()();
        let password = tst.password()();
        let cipher = tst.cipher()();

        let res: Result<(String, String), AppError> =
            (|| {
                let note_data = match cipher {
                    Some(cipher) => {
                        if password.is_empty() {
                            return Err(
                                AppError::PasswordRequired,
                            );
                        }
                        NoteData::CipherText(
                            encrypt_symmetric(
                                content.as_bytes(),
                                &password,
                                cipher,
                            )?,
                        )
                    }
                    None => NoteData::PlainText(content),
                };

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
                let u = build_url(&view_url, &note_data)?;
                let q = generate_qr_code(&u)?;

                Ok((u, q))
            })();

        match res {
            Ok((u, q)) => {
                url.set(u);
                qr_code.set(q);
                message.set(None);
            }
            Err(e) => message.set(Some(Msg::Error(e))),
        }
    });

    rsx! {
        Breadcrumb { title: Msg::ShareTitle }
        section {
            if !url().is_empty() {
                if !qr_code().is_empty() {
                    div { dangerous_inner_html: "{qr_code}" }
                }

                textarea {
                    readonly: true,
                    value: "{url}",
                    onclick: move |_| {
                        write_clipboard(url(), message);
                    },
                }

                Dock { message,
                    Button {
                        icon: Some(FaEye),
                        onclick: move |_| {
                            nav.write().push(Screen::View.to_route(None));
                        },
                        i18n: Some(Msg::ViewButton),
                        lang,
                    }
                    Button {
                        icon: Some(FaCopy),
                        primary: true,
                        onclick: move |_| {
                            write_clipboard(url(), message);
                        },
                        i18n: Some(Msg::Base(BaseMsg::Copy)),
                        lang,
                    }
                }
            } else if message.read().is_some() {
                Dock { message }
            } else {
                p { "{Msg::Base(BaseMsg::Loading).render(lang)}" }
            }
        }
    }
}
