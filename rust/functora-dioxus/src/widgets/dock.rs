use crate::Msg;
use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use crate::widgets::{Align, Button};
use dioxus::prelude::*;
use dioxus_free_icons::IconShape;
use dioxus_free_icons::icons::fa_solid_icons::FaArrowLeft;

#[component]
pub fn GenDock<
    R: 'static,
    M: I18N + 'static,
    B: I18N + Clone + PartialEq + 'static,
    I: IconShape + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
    S: Readable<Target = Option<M>> + Clone + PartialEq + 'static,
>(
    children: Element,
    nav: N,
    #[props(default)] message: Option<S>,
    #[props(default)] back_button_i18n: Option<B>,
    #[props(default)] back_button_icon: Option<I>,
    #[props(default)] back_button_hide: bool,
    lang: Language,
) -> Element {
    let has_navigated = nav.with(Nav::has_navigated);

    rsx! {
        if let Some(message) = message.filter(|m| m.with(Option::is_some)) {
            crate::widgets::Banner { message, lang, align: Some(Align::Right) }
        }
        footer {
            if has_navigated && !back_button_hide {
                Button {
                    icon: back_button_icon,
                    onclick: move |_| {
                        nav.write().go_back();
                    },
                    i18n: back_button_i18n,
                    lang,
                }
            }
            {children}
        }
    }
}

#[component]
pub fn Dock<
    R: 'static,
    M: I18N + Clone + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
    S: Readable<Target = Option<M>> + Clone + PartialEq + 'static,
>(
    children: Element,
    nav: N,
    #[props(default)] message: Option<S>,
    #[props(default = Some(Msg::Back))] back_button_i18n: Option<Msg>,
    #[props(default = Some(FaArrowLeft))] back_button_icon: Option<FaArrowLeft>,
    #[props(default)] back_button_hide: bool,
    lang: Language,
) -> Element {
    GenDock(GenDockProps {
        children,
        nav,
        message,
        back_button_i18n,
        back_button_icon,
        back_button_hide,
        lang,
    })
}
