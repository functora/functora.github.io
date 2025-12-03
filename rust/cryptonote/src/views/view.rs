use crate::crypto::decrypt_symmetric;
use crate::encoding::{NoteData, parse_url};
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;
use web_sys::window;

#[component]
pub fn View() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut encrypted_data = use_signal(|| {
        Option::<crate::crypto::EncryptedData>::None
    });
    let mut password_input = use_signal(String::new);
    let mut error_message =
        use_signal(|| Option::<String>::None);
    let mut is_encrypted = use_signal(|| false);

    use_effect(move || {
        let t = get_translations(language());
        if let Some(window) = window() {
            if let Ok(href) = window.location().href() {
                if href.contains("#note=") {
                    match parse_url(&href) {
                        Ok(note_data) => match note_data {
                            NoteData::CipherText(enc) => {
                                is_encrypted.set(true);
                                encrypted_data
                                    .set(Some(enc));
                            }
                            NoteData::PlainText(text) => {
                                note_content
                                    .set(Some(text));
                            }
                        },
                        Err(e) => error_message
                            .set(Some(e.localized(&t))),
                    }
                } else {
                    error_message.set(Some(
                        t.no_note_in_url.to_string(),
                    ));
                }
            }
        }
    });

    let mut app_context =
        use_context::<Signal<crate::AppContext>>();

    let decrypt_note = move |_evt: Event<MouseData>| {
        let t = get_translations(language());
        error_message.set(None);
        if let Some(enc) = encrypted_data.read().as_ref() {
            let pwd = password_input.read().clone();
            if pwd.is_empty() {
                error_message.set(Some(
                    t.password_required.to_string(),
                ));
                return;
            }

            match decrypt_symmetric(enc, &pwd) {
                Ok(plaintext) => {
                    match String::from_utf8(plaintext) {
                        Ok(text) => {
                            note_content
                                .set(Some(text.clone()));
                            is_encrypted.set(false);

                            app_context.set(
                                crate::AppContext {
                                    content: Some(text),
                                    password: pwd,
                                    cipher: Some(
                                        enc.cipher,
                                    ),
                                    share_url: None,
                                    qr_code: None,
                                },
                            );
                        }
                        Err(_) => error_message.set(Some(
                            t.invalid_utf8.to_string(),
                        )),
                    }
                }
                Err(e) => {
                    error_message.set(Some(e.localized(&t)))
                }
            }
        }
    };

    rsx! {
        if is_encrypted() {
            section {
                div {
                    a {
                        href: "#",
                        onclick: move |_| {
                            nav.push("/");
                        },
                        "{t.home}"
                    }
                    " > {t.encrypted_note}"
                }
                p { "{t.encrypted_note_desc}" }

                if let Some(enc) = encrypted_data() {
                    p {
                        strong { "{t.algorithm}: " }
                        match enc.cipher {
                            crate::crypto::CipherType::ChaCha20Poly1305 => "ChaCha20-Poly1305",
                            crate::crypto::CipherType::Aes256Gcm => "AES-256-GCM",
                        }
                    }
                }

                fieldset {
                    label { "{t.password}" }
                    input {
                        r#type: "password",
                        placeholder: "{t.password_placeholder}",
                        value: "{password_input}",
                        oninput: move |evt| password_input.set(evt.value()),
                        onkeydown: move |evt| {
                            if evt.key() == Key::Enter {
                                let t = get_translations(language());
                                error_message.set(None);
                                if let Some(enc) = encrypted_data.read().as_ref() {
                                    let pwd = password_input.read().clone();
                                    if !pwd.is_empty() {
                                        match decrypt_symmetric(enc, &pwd) {
                                            Ok(plaintext) => {
                                                if let Ok(text) = String::from_utf8(plaintext) {
                                                    note_content.set(Some(text.clone()));
                                                    is_encrypted.set(false);
                                                    app_context
                                                        .set(crate::AppContext {
                                                            content: Some(text),
                                                            password: pwd,
                                                            cipher: Some(enc.cipher),
                                                            share_url: None,
                                                            qr_code: None,
                                                        });
                                                } else {
                                                    error_message.set(Some(t.invalid_utf8.to_string()));
                                                }
                                            }
                                            Err(e) => {
                                                error_message.set(Some(e.localized(&t)));
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }

                    br {}
                    br {}

                    p {
                        button { onclick: decrypt_note, "{t.decrypt_button}" }

                        if let Some(err) = error_message() {
                            div { "{err}" }
                        }
                    }
                }
            }
        } else if let Some(content) = note_content() {
            section {
                div {
                    a {
                        href: "#",
                        onclick: move |_| {
                            nav.push("/");
                        },
                        "{t.home}"
                    }
                    " > {t.your_note_title}"
                }
                article {
                    pre { "{content}" }
                }

                p {
                    button {
                        onclick: move |_| {
                            if app_context.read().cipher.is_none() {
                                app_context
                                    .set(crate::AppContext {
                                        content: Some(content.clone()),
                                        password: String::new(),
                                        cipher: None,
                                        share_url: None,
                                        qr_code: None,
                                    });
                            }
                            nav.push("/");
                        },
                        "{t.edit_note}"
                    }
                    button {
                        onclick: move |_| {
                            app_context
                                .set(crate::AppContext {
                                    content: None,
                                    password: String::new(),
                                    cipher: None,
                                    share_url: None,
                                    qr_code: None,
                                });
                            nav.push("/");
                        },
                        "{t.create_new_note}"
                    }
                }
            }
        } else if let Some(err) = error_message() {
            section {
                div {
                    a {
                        href: "#",
                        onclick: move |_| {
                            nav.push("/");
                        },
                        "{t.home}"
                    }
                    " > {t.error_title}"
                }
                p { "{err}" }
                p {
                    button {
                        onclick: move |_| {
                            nav.push("/");
                        },
                        "{t.create_new_note}"
                    }
                }
            }
        } else {
            section {
                p { "{t.loading}" }
            }
        }
    }
}
