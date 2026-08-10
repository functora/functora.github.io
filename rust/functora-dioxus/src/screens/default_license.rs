#![allow(clippy::shadow_reuse)]
use crate::hooks::use_lang;
use crate::messages::Msg;
use crate::nav::Nav;
use crate::white_label::WhiteLabelContent;
use crate::widgets::{Dock, StaticPage};
use dioxus::prelude::*;

#[component]
pub fn DefaultLicense<R, N>(home_route: R, nav: N, #[props(default)] content: WhiteLabelContent<Msg>) -> Element
where
    R: Routable + Clone + Default + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
{
    let lang = use_lang();
    let text = content.license_text.unwrap_or(Msg::LicenseText);
    rsx! {
        StaticPage {
            title: Msg::TermsOfServiceTitle,
            content: text,
            home_label: Msg::Home,
            home_route,
            nav: nav.clone(),
            lang,
            children: rsx! {
                Dock { nav, lang }
            },
        }
    }
}
