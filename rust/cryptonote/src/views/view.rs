use crate::crypto::{EncryptionMode, decrypt_symmetric};
use crate::encoding::parse_url;
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;

#[component]
pub fn View() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut password_input = use_signal(|| String::new());
    let mut error_message =
        use_signal(|| Option::<String>::None);
    let mut encrypted_data = use_signal(|| {
        Option::<crate::crypto::EncryptedData>::None
    });
    let mut is_encrypted = use_signal(|| false);

    use_effect(move || {
        if let Some(window) = web_sys::window() {
            if let Ok(href) = window.location().href() {
                if href.contains("#note=") {
                    match parse_url(&href) {
                        Ok(note_data) => {
                            if let Some(enc) =
                                note_data.encrypted
                            {
                                is_encrypted.set(true);
                                encrypted_data
                                    .set(Some(enc));
                            } else {
                                match String::from_utf8(note_data.content) {
                                    Ok(text) => note_content.set(Some(text)),
                                    Err(_) => error_message.set(Some("Invalid UTF-8 in note".to_string())),
                                }
                            }
                        }
                        Err(e) => error_message.set(Some(
                            format!(
                                "{}: {}",
                                t.failed_to_parse_url, e
                            ),
                        )),
                    }
                } else {
                    error_message.set(Some(
                        t.no_note_in_url.to_string(),
                    ));
                }
            }
        }
    });

    let decrypt_note = move |_evt: Event<MouseData>| {
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
                Ok(plaintext) => match String::from_utf8(
                    plaintext,
                ) {
                    Ok(text) => {
                        note_content.set(Some(text));
                        is_encrypted.set(false);
                    }
                    Err(_) => error_message.set(Some(
                        t.invalid_utf8
                            .to_string(),
                    )),
                },
                Err(e) => error_message.set(Some(format!(
                    "{}: {}",
                    t.decryption_failed, e
                ))),
            }
        }
    };

    rsx! {
        div {
            h1 { "Cryptonote" }

            if is_encrypted() {
                div {
                    h2 { "{t.encrypted_note}" }
                    p { "{t.encrypted_note_desc}" }

                    if let Some(enc) = encrypted_data.read().as_ref() {
                        div {
                            p {
                                "Cipher: "
                                strong {
                                    match enc.mode {
                                        EncryptionMode::Symmetric { cipher } => {
                                            match cipher {
                                                crate::crypto::CipherType::ChaCha20Poly1305 => "ChaCha20-Poly1305",
                                                crate::crypto::CipherType::Aes256Gcm => "AES-256-GCM",
                                            }
                                        }
                                        _ => "Unknown",
                                    }
                                }
                            }
                        }
                    }

                    div {
                        input {
                            r#type: "password",
                            placeholder: "Enter password",
                            value: "{password_input}",
                            oninput: move |evt| password_input.set(evt.value()),
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    error_message.set(None);
                                    if let Some(enc) = encrypted_data.read().as_ref() {
                                        let pwd = password_input.read().clone();
                                        if !pwd.is_empty() {
                                            if let Ok(plaintext) = decrypt_symmetric(enc, &pwd) {
                                                if let Ok(text) = String::from_utf8(plaintext) {
                                                    note_content.set(Some(text));
                                                    is_encrypted.set(false);
                                                }
                                            } else {
                                                error_message.set(Some("Decryption failed".to_string()));
                                            }
                                        }
                                    }
                                }
                            },
                        }
                        button { onclick: decrypt_note, "{t.decrypt_button}" }
                    }

                    if let Some(err) = error_message() {
                        div { "{err}" }
                    }
                }
            } else if let Some(content) = note_content() {
                div {
                    h2 { "{t.your_note_title}" }
                    div {
                        pre { "{content}" }
                    }
                    a { href: "/", "Create a new note" }
                }
            } else if let Some(err) = error_message() {
                div {
                    h2 { "{t.error_title}" }
                    p { "{err}" }
                    a { href: "/", "Create a new note" }
                }
            } else {
                div {
                    p { "{t.loading}" }
                }
            }
        }
    }
}
