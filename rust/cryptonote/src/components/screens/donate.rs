#![allow(clippy::shadow_reuse)]
use crate::messages::*;
use crate::*;

const BTC_ADDRESS: &str = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str =
    "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

#[component]
fn CryptoDonateBlock(
    label: &'static str,
    address: &'static str,
    qr: Option<String>,
    #[props(default)] back_button_hide: bool,
) -> Element {
    let lang = use_lang();
    let message = use_message();

    rsx! {
        section {
            h3 { "{label}" }

            if let Some(qr) = qr {
                div { dangerous_inner_html: "{qr}" }
            }

            textarea {
                readonly: true,
                rows: "2",
                value: "{address}",
                onclick: move |_| {
                    write_clipboard(address.to_string(), message);
                },
            }

            Dock { message, back_button_hide,
                Button {
                    icon: Some(FaCopy),
                    primary: true,
                    onclick: move |_| {
                        write_clipboard(address.to_string(), message);
                    },
                    i18n: Some(Msg::Base(BaseMsg::Copy)),
                    lang,
                }
            }
        }
    }
}

#[component]
pub fn Donate() -> Element {
    let lang = use_lang();
    let btc_qr = use_memo(|| generate_qr_code(BTC_ADDRESS).ok());
    let xmr_qr = use_memo(|| generate_qr_code(XMR_ADDRESS).ok());

    rsx! {
        Breadcrumb { title: Msg::Donate }
        section { "fs": "l", "{Msg::DonateGreeting.render(lang)} {Msg::DonateIntro.render(lang)}" }

        CryptoDonateBlock {
            label: "BTC - Bitcoin",
            address: BTC_ADDRESS,
            qr: btc_qr(),
            back_button_hide: true,
        }

        CryptoDonateBlock { label: "XMR - Monero", address: XMR_ADDRESS, qr: xmr_qr() }
    }
}
