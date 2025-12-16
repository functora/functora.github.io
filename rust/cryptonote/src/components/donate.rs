use crate::*;
use qrcode::{QrCode, render::svg};

const BTC_ADDRESS: &str =
    "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

async fn copy_to_clipboard(
    text: String,
) -> Result<(), AppError> {
    let mut eval = document::eval(
        r#"
        let msg = await dioxus.recv();
        try {
            await window.navigator.clipboard.writeText(msg);
            dioxus.send("ok");
        } catch (e) {
            dioxus.send("Error " + e);
        }
        "#,
    );

    eval.send(text).map_err(|e| {
        AppError::ClipboardWrite(e.to_string())
    })?;

    let msg = eval.recv::<String>().await.map_err(|e| {
        AppError::ClipboardWrite(e.to_string())
    })?;

    match msg.as_str() {
        "ok" => Ok(()),
        e => Err(AppError::ClipboardWrite(e.to_string())),
    }
}

fn generate_crypto_qr(address: &str) -> Option<String> {
    QrCode::new(address).ok().map(|code| {
        code.render::<svg::Color>()
            .min_dimensions(200, 200)
            .build()
    })
}

#[component]
pub fn Donate() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());

    let mut btc_message =
        use_signal(|| Option::<UiMessage>::None);
    let mut xmr_message =
        use_signal(|| Option::<UiMessage>::None);

    let btc_qr = generate_crypto_qr(BTC_ADDRESS);
    let xmr_qr = generate_crypto_qr(XMR_ADDRESS);

    rsx! {
        Breadcrumb { title: t.donate_title.to_string() }
        section {
            h3 { "{t.donate_greeting}" }
            article { font_size: "larger", "{t.donate_intro}" }

            br {}

            fieldset {
                h3 { "BTC - Bitcoin" }

                if let Some(qr) = btc_qr {
                    div { dangerous_inner_html: "{qr}" }
                }

                textarea {
                    readonly: true,
                    rows: "2",
                    value: "{BTC_ADDRESS}",
                    onclick: move |_| {
                        let addr = BTC_ADDRESS.to_string();
                        spawn(async move {
                            match copy_to_clipboard(addr).await {
                                Ok(_) => btc_message.set(Some(UiMessage::Copied)),
                                Err(e) => btc_message.set(Some(UiMessage::Error(e))),
                            }
                        });
                    },
                }

                ActionRow { message: btc_message,
                    button {
                        "primary": "",
                        onclick: move |_| {
                            let addr = BTC_ADDRESS.to_string();
                            spawn(async move {
                                match copy_to_clipboard(addr).await {
                                    Ok(_) => btc_message.set(Some(UiMessage::Copied)),
                                    Err(e) => btc_message.set(Some(UiMessage::Error(e))),
                                }
                            });
                        },
                        "{t.copy_button}"
                    }
                }
            }

            br {}

            fieldset {
                h3 { "XMR - Monero" }

                if let Some(qr) = xmr_qr {
                    div { dangerous_inner_html: "{qr}" }
                }

                textarea {
                    readonly: true,
                    rows: "2",
                    value: "{XMR_ADDRESS}",
                    onclick: move |_| {
                        let addr = XMR_ADDRESS.to_string();
                        spawn(async move {
                            match copy_to_clipboard(addr).await {
                                Ok(_) => xmr_message.set(Some(UiMessage::Copied)),
                                Err(e) => xmr_message.set(Some(UiMessage::Error(e))),
                            }
                        });
                    },
                }

                ActionRow { message: xmr_message,
                    button {
                        "primary": "",
                        onclick: move |_| {
                            let addr = XMR_ADDRESS.to_string();
                            spawn(async move {
                                match copy_to_clipboard(addr).await {
                                    Ok(_) => xmr_message.set(Some(UiMessage::Copied)),
                                    Err(e) => xmr_message.set(Some(UiMessage::Error(e))),
                                }
                            });
                        },
                        "{t.copy_button}"
                    }
                }
            }
        }
    }
}
