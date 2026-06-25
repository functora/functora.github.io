use crate::messages::*;
use crate::*;

const BTC_ADDRESS: &str =
    "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

#[component]
fn CryptoDonateBlock(
    label: &'static str,
    address: &'static str,
    qr: Option<String>,
) -> Element {
    let lang = use_lang();
    let message = use_message();

    rsx! {
        fieldset {
            h3 { "{label}" }

            if let Some(qr) = qr {
                div { dangerous_inner_html: "{qr}" }
            }

            textarea {
                readonly: true,
                rows: "2",
                value: "{address}",
                onclick: move |_| {
                    write_clipboard(address.to_string(), message, Msg::Copied, |_e| Msg::ClipboardWriteError);
                },
            }

            Dock { message,
                Button {
                    icon: FaCopy,
                    primary: true,
                    onclick: move |_| {
                        write_clipboard(address.to_string(), message, Msg::Copied, |_e| Msg::ClipboardWriteError);
                    },
                    "{Msg::Copy.render(lang)}"
                }
            }
        }
    }
}

#[component]
pub fn Donate() -> Element {
    let lang = use_lang();

    rsx! {
        Breadcrumb { title: Msg::DonateTitle }
        section {
            fieldset {
                h3 { "{Msg::DonateGreeting.render(lang)}" }
                article { font_size: "larger", "{Msg::DonateIntro.render(lang)}" }
            }

            br {}

            CryptoDonateBlock {
                label: "BTC - Bitcoin",
                address: BTC_ADDRESS,
                qr: generate_qr_code(BTC_ADDRESS).ok(),
            }

            br {}

            CryptoDonateBlock {
                label: "XMR - Monero",
                address: XMR_ADDRESS,
                qr: generate_qr_code(XMR_ADDRESS).ok(),
            }
        }
    }
}
