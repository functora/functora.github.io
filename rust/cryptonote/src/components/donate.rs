use crate::*;
use qrcode::{QrCode, render::svg};

const BTC_ADDRESS: &str =
    "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

fn generate_crypto_qr(address: &str) -> Option<String> {
    QrCode::new(address).ok().map(|code| {
        code.render::<svg::Color>()
            .min_dimensions(200, 200)
            .build()
    })
}

#[component]
pub fn Donate() -> Element {
    let app_settings = use_context::<Signal<AppSettings>>();
    let t = get_translations(app_settings.read().language);

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

                textarea { readonly: true, rows: "2", value: "{BTC_ADDRESS}" }

                Dock { message: btc_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: move |_| {
                            let addr = BTC_ADDRESS.to_string();
                            spawn(async move {
                                match js_write_clipboard(addr).await {
                                    Ok(()) => btc_message.set(Some(UiMessage::Copied)),
                                    Err(e) => {
                                        btc_message
                                            .set(Some(UiMessage::Error(AppError::JsWriteClipboard(e))))
                                    }
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

                textarea { readonly: true, rows: "2", value: "{XMR_ADDRESS}" }

                Dock { message: xmr_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: move |_| {
                            let addr = XMR_ADDRESS.to_string();
                            spawn(async move {
                                match js_write_clipboard(addr).await {
                                    Ok(()) => xmr_message.set(Some(UiMessage::Copied)),
                                    Err(e) => {

                                        xmr_message
                                            .set(Some(UiMessage::Error(AppError::JsWriteClipboard(e))))
                                    }
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
