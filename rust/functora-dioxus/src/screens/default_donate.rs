#![allow(clippy::shadow_reuse)]
use crate::encoding::generate_qr_code;
use crate::hooks::{use_lang, use_message};
use crate::i18n::{I18N, Language};
use crate::messages::Msg;
use crate::nav::Nav;
use crate::white_label::WhiteLabelContent;
use crate::widgets::{Breadcrumb, Button, Dock};
use crate::write_clipboard;
use dioxus::prelude::*;
use dioxus_free_icons::icons::fa_solid_icons::FaCopy;

#[component]
pub fn DefaultDonate<R, N>(home_route: R, nav: N, #[props(default)] content: WhiteLabelContent<Msg>) -> Element
where
    R: Routable + Clone + Default + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
{
    let lang = use_lang();
    let message = use_message::<Msg>();
    let greeting = content
        .donate_greeting
        .as_ref()
        .map_or_else(|| Msg::DonateGreeting.render(lang), |m| m.render(lang));
    let intro = content
        .donate_intro
        .as_ref()
        .map_or_else(|| Msg::DonateIntro.render(lang), |m| m.render(lang));
    rsx! {
        Breadcrumb {
            title: Msg::Donate,
            home_label: Msg::Home,
            home_route,
            nav: nav.clone(),
            lang,
        }
        section { "fs": "l", "{greeting} {intro}" }
        for (i, block) in content.donate_blocks.iter().enumerate() {
            CryptoDonateBlock {
                label: block.label.clone(),
                address: block.address.clone(),
                nav: nav.clone(),
                message,
                lang,
                back_button_hide: i + 1 < content.donate_blocks.len(),
            }
        }
    }
}

#[component]
fn CryptoDonateBlock<R, N>(
    label: String,
    address: String,
    nav: N,
    message: Signal<Option<Msg>>,
    lang: Language,
    #[props(default)] back_button_hide: bool,
) -> Element
where
    R: Routable + Clone + Default + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
{
    let address = use_signal(|| address);
    let qr = use_memo(move || generate_qr_code(&address()).ok());
    let copy_msg = message;
    let button_msg = copy_msg;
    rsx! {
        section {
            h3 { "{label}" }

            if let Some(qr) = qr() {
                div { dangerous_inner_html: "{qr}" }
            }

            textarea {
                readonly: true,
                rows: "2",
                value: "{address()}",
                onclick: move |_| {
                    write_clipboard(address(), copy_msg, Msg::Copied, |e| {
                        Msg::ClipboardWriteError(e.to_string())
                    });
                },
            }

            Dock {
                nav,
                message: Some(message),
                lang,
                back_button_hide,
                Button {
                    icon: Some(FaCopy),
                    primary: true,
                    onclick: move |_| {
                        write_clipboard(address(), button_msg, Msg::Copied, |e| {
                            Msg::ClipboardWriteError(e.to_string())
                        });
                    },
                    i18n: Some(Msg::Copy),
                    lang,
                }
            }
        }
    }
}
