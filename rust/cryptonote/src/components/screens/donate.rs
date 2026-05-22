use crate::*;

const BTC_ADDRESS: &str =
    "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

fn generate_crypto_qr(address: &str) -> Option<String> {
    generate_qr_code(address).ok()
}

#[component]
pub fn Donate() -> Element {
    let t = use_translations();

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
                    onclick: move |_| {
                        write_clipboard(BTC_ADDRESS.to_string(), btc_message);
                    },
                }

                Dock { message: btc_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: move |_| {
                            write_clipboard(BTC_ADDRESS.to_string(), btc_message);
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
                        write_clipboard(XMR_ADDRESS.to_string(), xmr_message);
                    },
                }

                Dock { message: xmr_message,
                    Button {
                        icon: FaPenToSquare,
                        primary: true,
                        onclick: move |_| {
                            write_clipboard(XMR_ADDRESS.to_string(), xmr_message);
                        },
                        "{t.copy_button}"
                    }
                }
            }
        }
    }
}
