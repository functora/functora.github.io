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
    let mut encryption_mode =
        use_signal(|| "none".to_string());
    let mut cipher_type =
        use_signal(|| "chacha20".to_string());
    let mut password = use_signal(|| String::new());
    let mut generated_url =
        use_signal(|| Option::<String>::None);
    let mut qr_code_svg =
        use_signal(|| Option::<String>::None);
    let mut error_message =
        use_signal(|| Option::<String>::None);

    let generate_note = move |_| {
        error_message.set(None);
        generated_url.set(None);
        qr_code_svg.set(None);

        let note_content = note_text.read().clone();
        let enc_mode = encryption_mode.read().clone();
        let cipher =
            if cipher_type.read().as_str() == "chacha20" {
                CipherType::ChaCha20Poly1305
            } else {
                CipherType::Aes256Gcm
            };

        let note_data = match enc_mode.as_str() {
            "none" => NoteData {
                content: note_content.into_bytes(),
                encrypted: None,
            },
            "symmetric" => {
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
                    Ok(encrypted) => NoteData {
                        content: vec![],
                        encrypted: Some(encrypted),
                    },
                    Err(e) => {
                        error_message.set(Some(format!(
                            "{}: {}",
                            t.encryption_failed, e
                        )));
                        return;
                    }
                }
            }
            _ => {
                error_message.set(Some("Asymmetric encryption not yet implemented".to_string()));
                return;
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
                        error_message.set(Some(format!(
                            "{}: {}",
                            t.qr_generation_failed, e
                        )))
                    }
                }
                generated_url.set(Some(url));
            }
            Err(e) => error_message.set(Some(format!(
                "{}: {}",
                t.url_generation_failed, e
            ))),
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

                input {
                    r#type: "radio",
                    id: "enc_none",
                    name: "encryption",
                    value: "none",
                    checked: encryption_mode() == "none",
                    onchange: move |_| encryption_mode.set("none".to_string()),
                }
                label { r#for: "enc_none", "{t.no_encryption}" }

                br {}

                input {
                    r#type: "radio",
                    id: "enc_symmetric",
                    name: "encryption",
                    value: "symmetric",
                    checked: encryption_mode() == "symmetric",
                    onchange: move |_| encryption_mode.set("symmetric".to_string()),
                }
                label { r#for: "enc_symmetric", "{t.password_encryption}" }

                if encryption_mode() == "symmetric" {

                    label { "{t.cipher}" }
                    select {
                        value: "{cipher_type}",
                        onchange: move |evt| cipher_type.set(evt.value()),
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
