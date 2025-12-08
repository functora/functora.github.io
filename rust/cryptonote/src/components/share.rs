use crate::Route;
use crate::components::Breadcrumb;
use crate::components::actions::ActionRow;
use crate::components::message::UiMessage;
use crate::i18n::{Language, get_translations};
use crate::prelude::*;
use dioxus_clipboard::prelude::use_clipboard;

#[cfg(not(target_arch = "wasm32"))]
fn copy_to_clipboard(
    text: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let _ = use_clipboard().set(text.into());
    Ok(())
}

#[cfg(target_arch = "wasm32")]
fn copy_to_clipboard(
    text: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(window) = web_sys::window() {
        let clipboard = window.navigator().clipboard();
        let _ = clipboard.write_text(text);
    }
    Ok(())
}

#[component]
pub fn Share() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();

    let mut url = use_signal(|| String::new());
    let mut qr_code = use_signal(|| String::new());
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);
    let app_context =
        use_context::<Signal<crate::AppContext>>();

    use_effect(move || {
        let ctx = app_context.read();

        if let Some(share_url) = &ctx.share_url {
            url.set(share_url.clone());
        }
        if let Some(qr) = &ctx.qr_code {
            qr_code.set(qr.clone());
        }
        if let Some(content) = &ctx.content {
            note_content.set(Some(content.clone()));
        }

        if ctx.share_url.is_none() {
            nav.push(Route::Home {});
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
                            let _ = copy_to_clipboard(&url_val);
                            message.set(Some(UiMessage::Copied));
                        },
                    }

                    ActionRow { message,
                        button {
                            "primary": "",
                            onclick: move |_| {
                                let url_val = url();
                                let _ = copy_to_clipboard(&url_val);
                                message.set(Some(UiMessage::Copied));
                            },
                            "{t.copy_button}"
                        }
                    }
                }
            } else {
                p { "{t.loading}" }
            }
        }
    }
}
