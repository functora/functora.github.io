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

    let mut note_text = use_signal(|| String::new());
    let mut encryption = use_signal(|| Option::<CipherType>::None);
    let mut password = use_signal(|| String::new());
    let mut generated_url =
        use_signal(|| Option::<String>::None);
    let mut qr_code_svg =
        use_signal(|| Option::<String>::None);
    let mut error_message =
        use_signal(|| Option::<String>::None);

    let generate_note = move |_| {
        let t = get_translations(language());
        error_message.set(None);
        generated_url.set(None);
        qr_code_svg.set(None);

        let note_content = note_text.read().clone();
        let enc_option = encryption.read().clone();

        let note_data = match enc_option {
            None => NoteData::PlainText(note_content),
            Some(cipher) => {
                let pwd = password.read().clone();
                if pwd.is_empty() {
                    error_message.set(Some(
                        t.password_required.to_string(),
                    ));
                    return;
                }
                match encrypt_symmetric(
                    note_content.as_bytes(),
                    &pwd,
                    cipher,
                ) {
                    Ok(encrypted) => NoteData::CipherText(encrypted),
                    Err(e) => {
                        error_message.set(Some(
                            e.localized(&t)
                        ));
                        return;
                    }
                }
            }
        };

        let base_url = web_sys::window()
            .and_then(|w| w.location().href().ok())
            .unwrap_or_else(|| {
                "http://localhost:8080".to_string()
            })
            .split('#')
            .next()
            .unwrap_or("http://localhost:8080")
            .to_string()
            + "view";

        match build_url(&base_url, &note_data) {
            Ok(url) => {
                match generate_qr_code(&url) {
                    Ok(svg) => qr_code_svg.set(Some(svg)),
                    Err(e) => {
                        error_message.set(Some(
                            e.localized(&t)
                        ))
                    }
                }
                generated_url.set(Some(url));
            }
            Err(e) => error_message.set(Some(
                e.localized(&t)
            )),
        }
    };

    rsx! {
        section {
            fieldset {

                label { "{t.password}" }
                textarea {
                    placeholder: "{t.note_placeholder}",
                    rows: "8",
                    value: "{note_text}",
                    oninput: move |evt| note_text.set(evt.value()),
                }

                br {}

                input {
                    r#type: "radio",
                    id: "enc_none",
                    name: "encryption",
                    value: "none",
                    checked: encryption().is_none(),
                    onchange: move |_| encryption.set(None),
                }
                label { r#for: "enc_none", "{t.no_encryption}" }

                br {}

                input {
                    r#type: "radio",
                    id: "enc_symmetric",
                    name: "encryption",
                    value: "symmetric",
                    checked: encryption().is_some(),
                    onchange: move |_| encryption.set(Some(CipherType::ChaCha20Poly1305)),
                }
                label { r#for: "enc_symmetric", "{t.password_encryption}" }

                if let Some(cipher) = encryption() {

                    label { "{t.cipher}" }
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

                p {
                    button { onclick: generate_note, "{t.generate_button}" }

                    if let Some(err) = error_message() {
                        div { "{err}" }
                    }
                }
            }
        }


        if let Some(url) = generated_url() {
            section {
                h2 { "{t.share_title}" }
                div {
                    input {
                        r#type: "text",
                        readonly: true,
                        value: "{url}",
                        onclick: move |_| {
                            if let Some(window) = web_sys::window() {
                                let clipboard = window.navigator().clipboard();
                                let _ = clipboard.write_text(&url);
                            }
                        },
                    }
                    p { "{t.click_to_copy}" }
                }

                if let Some(svg) = qr_code_svg() {
                    figure {
                        h3 { "{t.qr_code}" }
                        div { dangerous_inner_html: "{svg}" }
                    }
                }
            }
        }
    }
}
