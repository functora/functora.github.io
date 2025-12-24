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

fn onclick(
    addr: &'static str,
    mut message: Signal<Option<UiMessage>>,
) -> impl FnMut(Event<MouseData>) {
    move |_: Event<MouseData>| {
        spawn(async move {
            match js_write_clipboard(addr.to_string()).await
            {
                Ok(()) => {
                    message.set(Some(UiMessage::Copied))
                }
                Err(e) => {
                    message.set(Some(UiMessage::Error(
                        AppError::JsWriteClipboard(e),
                    )))
                }
            }
        });
    }
}

#[component]
pub fn Donate() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);

    let btc_message =
        use_signal(|| Option::<UiMessage>::None);
    let xmr_message =
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
                    onclick: onclick(BTC_ADDRESS, btc_message),
                }

                Dock { message: btc_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: onclick(BTC_ADDRESS, btc_message),
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
                    onclick: onclick(XMR_ADDRESS, xmr_message),
                }

                Dock { message: xmr_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: onclick(XMR_ADDRESS, xmr_message),
                        "{t.copy_button}"
                    }
                }
            }
        }
    }
}
