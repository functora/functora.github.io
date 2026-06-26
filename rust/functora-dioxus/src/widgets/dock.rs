use crate::i18n::{Language, I18N};
use crate::nav::Nav;
use crate::widgets::Button;
use crate::Msg;
use dioxus::prelude::*;
use dioxus_free_icons::icons::fa_solid_icons::FaArrowLeft;
use dioxus_free_icons::IconShape;

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
    #[props(default)] lang: Language,
) -> Element {
    let has_navigated = nav.with(Nav::has_navigated);

    rsx! {
        if let Some(message) = message {
            p { "txt": "r",
                crate::widgets::Banner { message, lang }
            }
        }
        div { style: "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; justify-content: center;",
            if has_navigated {
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
    #[props(default)] lang: Language,
) -> Element {
    GenDock(GenDockProps {
        children,
        nav,
        message,
        back_button_i18n,
        back_button_icon,
        lang,
    })
}
