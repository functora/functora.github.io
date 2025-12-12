use crate::Screen;
use crate::components::actions::ActionRow;
use crate::components::message::UiMessage;
use crate::crypto::{CipherType, encrypt_symmetric};
use crate::encoding::{
    NoteData, build_url, generate_qr_code,
};
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;

#[component]
pub fn Home() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = use_navigator();
    let mut app_context =
        use_context::<Signal<crate::AppContext>>();

    let mut message =
        use_signal(|| Option::<UiMessage>::None);

    let generate_note = move |_| {
        message.set(None);

        let (note_content, enc_option, pwd) = {
            let ctx = app_context.read();
            (
                ctx.content.clone().unwrap_or_default(),
                ctx.cipher,
                ctx.password.clone(),
            )
        };

        let note_data = match enc_option {
            None => {
                NoteData::PlainText(note_content.clone())
            }
            Some(cipher) => {
                if pwd.is_empty() {
                    message.set(Some(UiMessage::Error(
                        crate::error::AppError::PasswordRequired,
                    )));
                    return;
                }
                match encrypt_symmetric(
                    note_content.as_bytes(),
                    &pwd,
                    cipher,
                ) {
                    Ok(encrypted) => {
                        NoteData::CipherText(encrypted)
                    }
                    Err(e) => {
                        message
                            .set(Some(UiMessage::Error(e)));
                        return;
                    }
                }
            }
        };

        let origin = {
            #[cfg(target_arch = "wasm32")]
            {
                web_sys::window().and_then(|w| {
                    let loc = w.location();
                    let protocol = loc.protocol().ok()?;
                    let host = loc.host().ok()?;
                    let pathname = loc.pathname().ok()?;
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
        };

        if let Some(origin) = origin {
            let view_url = format!(
                "{}/?screen={}",
                origin,
                crate::Screen::View
            );
            match build_url(&view_url, &note_data) {
                Ok(url) => match generate_qr_code(&url) {
                    Ok(qr) => {
                        app_context.set(
                            crate::AppContext {
                                content: Some(note_content),
                                password: pwd,
                                cipher: enc_option,
                                share_url: Some(url),
                                qr_code: Some(qr),
                            },
                        );

                        let mut nav_state = use_context::<
                            Signal<crate::NavigationState>,
                        >(
                        );
                        nav_state.write().has_navigated =
                            true;
                        nav.push(
                            Screen::Share.to_route(None),
                        );
                    }
                    Err(e) => message
                        .set(Some(UiMessage::Error(e))),
                },
                Err(e) => {
                    message.set(Some(UiMessage::Error(e)))
                }
            }
        }
    };

    rsx! {
        section {
            fieldset {

                label { "{t.note}" }
                textarea {
                    placeholder: "{t.note_placeholder}",
                    rows: "8",
                    value: "{app_context.read().content.clone().unwrap_or_default()}",
                    oninput: move |evt| {
                        let mut ctx = app_context.write();
                        ctx.content = Some(evt.value());
                    },
                }

                label { "{t.mode}" }

                input {
                    r#type: "radio",
                    value: "none",
                    checked: app_context.read().cipher.is_none(),
                    onchange: move |_| {
                        let mut ctx = app_context.write();
                        ctx.cipher = None;
                    },
                }
                label { "{t.no_encryption}" }

                br {}

                input {
                    r#type: "radio",
                    value: "symmetric",
                    checked: app_context.read().cipher.is_some(),
                    onchange: move |_| {
                        let mut ctx = app_context.write();
                        ctx.cipher = Some(CipherType::ChaCha20Poly1305);
                    },
                }
                label { "{t.password_encryption}" }

                if let Some(cipher) = app_context.read().cipher {

                    br {}
                    br {}

                    label { "{t.algorithm}" }
                    select {
                        value: match cipher {
                            CipherType::ChaCha20Poly1305 => "chacha20",
                            CipherType::Aes256Gcm => "aes",
                        },
                        onchange: move |evt| {
                            let new_cipher = match evt.value().as_str() {
                                "aes" => CipherType::Aes256Gcm,
                                _ => CipherType::ChaCha20Poly1305,
                            };
                            let mut ctx = app_context.write();
                            ctx.cipher = Some(new_cipher);
                        },
                        option { value: "chacha20", "ChaCha20-Poly1305" }
                        option { value: "aes", "AES-256-GCM" }
                    }

                    label { "{t.password}" }
                    input {
                        r#type: "password",
                        placeholder: "{t.password_placeholder}",
                        value: "{app_context.read().password}",
                        oninput: move |evt| {
                            let mut ctx = app_context.write();
                            ctx.password = evt.value();
                        },
                    }
                }

                br {}
                br {}

                ActionRow { message,
                    button {
                        onclick: move |_| {
                            message.set(None);
                            app_context.set(crate::AppContext::default());
                        },
                        "{t.create_new_note}"
                    }
                    button {
                        "primary": "",
                        onclick: move |_| {
                            let mut nav_state = use_context::<Signal<crate::NavigationState>>();
                            nav_state.write().has_navigated = true;
                            nav.push(Screen::Open.to_route(None));
                        },
                        "{t.open_button}"
                    }
                    button { "primary": "", onclick: generate_note, "{t.generate_button}" }
                }
            }
        }
    }
}
