use crate::*;

#[component]
pub fn Share() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();

    let mut url = use_signal(String::new);
    let mut qr_code = use_signal(String::new);
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);
    let app_context = use_context::<Signal<AppContext>>();

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
            nav.push(Screen::Home.to_route(None));
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

                    ActionRow { message,
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
            } else {
                p { "{t.loading}" }
            }
        }
    }
}
