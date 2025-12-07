use crate::crypto::{CipherType, encrypt_symmetric};
use crate::Route;
use crate::encoding::{
    NoteData, build_url, generate_qr_code,
};
use crate::i18n::{Language, get_translations};
use crate::components::actions::ActionRow;
use crate::components::message::UiMessage;
use dioxus::prelude::*;
use dioxus_router::prelude::navigator;

#[component]
pub fn Home() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let mut app_context =
        use_context::<Signal<crate::AppContext>>();

    let mut note_text = use_signal(|| String::new());
    let mut encryption =
        use_signal(|| Option::<CipherType>::None);
    let mut password = use_signal(|| String::new());
    let mut error_message =
        use_signal(|| Option::<UiMessage>::None);

    use_effect(move || {
        let ctx = app_context.read();
        if let Some(content) = &ctx.content {
            note_text.set(content.clone());
        }
        if !ctx.password.is_empty() {
            password.set(ctx.password.clone());
        }
        if let Some(cipher) = ctx.cipher {
            encryption.set(Some(cipher));
        }
    });

    let generate_note = move |_| {
        error_message.set(None);

        let note_content = note_text.read().clone();
        let enc_option = encryption.read().clone();
        let pwd = password.read().clone();

        let note_data = match enc_option {
            None => {
                NoteData::PlainText(note_content.clone())
            }
            Some(cipher) => {
                if pwd.is_empty() {
                    error_message.set(Some(UiMessage::Error(
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
                        error_message
                            .set(Some(UiMessage::Error(e)));
                        return;
                    }
                }
            }
        };

        if let Some(window) = web_sys::window() {
            if let Ok(origin) = window.location().origin() {
                let view_url = format!("{}/view", origin);
                match build_url(&view_url, &note_data) {
                    Ok(url) => match generate_qr_code(&url)
                    {
                        Ok(qr) => {
                            app_context.set(
                                crate::AppContext {
                                    content: Some(
                                        note_content,
                                    ),
                                    password: pwd,
                                    cipher: enc_option,
                                    share_url: Some(url),
                                    qr_code: Some(qr),
                                },
                            );

                            let mut nav_state = use_context::<
                                Signal<crate::NavigationState>,
                            >();
                            nav_state.write().has_navigated =
                                true;
                            nav.push(Route::Share {});
                        }
                        Err(e) => error_message.set(Some(
                            UiMessage::Error(e.into()),
                        )),
                    },
                    Err(e) => error_message.set(Some(
                        UiMessage::Error(e.into()),
                    )),
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
                    value: "{note_text}",
                    oninput: move |evt| note_text.set(evt.value()),
                }

                label { "{t.mode}" }

                input {
                    r#type: "radio",
                    value: "none",
                    checked: encryption().is_none(),
                    onchange: move |_| encryption.set(None),
                }
                label { "{t.no_encryption}" }

                br {}

                input {
                    r#type: "radio",
                    value: "symmetric",
                    checked: encryption().is_some(),
                    onchange: move |_| encryption.set(Some(CipherType::ChaCha20Poly1305)),
                }
                label { "{t.password_encryption}" }

                if let Some(cipher) = encryption() {

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
                            encryption.set(Some(new_cipher));
                        },
                        option { value: "chacha20", "ChaCha20-Poly1305" }
                        option { value: "aes", "AES-256-GCM" }
                    }

                    label { "{t.password}" }
                    input {
                        r#type: "password",
                        placeholder: "{t.password_placeholder}",
                        value: "{password}",
                        oninput: move |evt| password.set(evt.value()),
                    }
                }

                br {}
                br {}

                ActionRow { message: error_message,
                    button {
                        onclick: move |_| {
                            note_text.set(String::new());
                            encryption.set(None);
                            password.set(String::new());
                            error_message.set(None);
                            app_context.set(
                                crate::AppContext::default(),
                            );
                        },
                        "{t.create_new_note}"
                    }
                    button { "primary": "", onclick: generate_note, "{t.generate_button}" }
                }
            }
        }
    }
}
