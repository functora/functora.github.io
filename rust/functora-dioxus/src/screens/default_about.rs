#![allow(clippy::shadow_reuse)]
use crate::hooks::{use_lang, use_message_markdown};
use crate::i18n::I18N;
use crate::messages::Msg;
use crate::nav::Nav;
use crate::widgets::{Breadcrumb, Dock};
use dioxus::prelude::*;

#[component]
pub fn DefaultAbout<R, M, N>(about: M, home_route: R, nav: N) -> Element
where
    R: Routable + Clone + Default + PartialEq + 'static,
    M: I18N + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
{
    let lang = use_lang();
    let rendered = use_message_markdown(about);
    rsx! {
        Breadcrumb {
            title: Msg::Application,
            home_label: Msg::Home,
            home_route,
            nav: nav.clone(),
            lang,
        }
        section {
            p { dangerous_inner_html: "{rendered()}" }
        }
        Dock { nav, lang }
    }
}
