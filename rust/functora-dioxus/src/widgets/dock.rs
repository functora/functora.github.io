use crate::Msg;
use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use dioxus::prelude::*;
use dioxus_free_icons::Icon;
use dioxus_free_icons::icons::fa_solid_icons::FaArrowLeft;

#[component]
pub fn GenDock<R: 'static, M: I18N + Clone + PartialEq + 'static, B: I18N + Clone + PartialEq + 'static>(
    children: Element,
    nav: Signal<Nav<R>>,
    #[props(default)] message: Option<Signal<Option<M>>>,
    #[props(default)] back_button_i18n: Option<B>,
    #[props(default = Some(rsx! { Icon { icon: FaArrowLeft } }))] back_button_icon: Option<Element>,
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
                button {
                    onclick: move |_| {
                        nav.write().go_back();
                    },
                    {back_button_icon}
                    {back_button_i18n.map(|i18n| rsx! {
                        " "
                        {i18n.render(lang)}
                    })}
                }
            }
            {children}
        }
    }
}

#[component]
pub fn Dock<R: 'static, M: I18N + Clone + PartialEq + 'static>(
    children: Element,
    nav: Signal<Nav<R>>,
    #[props(default)] message: Option<Signal<Option<M>>>,
    #[props(default = Some(Msg::Back))] back_button_i18n: Option<Msg>,
    #[props(default = Some(rsx! { Icon { icon: FaArrowLeft } }))] back_button_icon: Option<Element>,
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
