use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use crate::widgets::banner::Banner;
use dioxus::prelude::*;

#[allow(clippy::redundant_closure_for_method_calls)]
#[component]
pub fn Dock<R: 'static, M: I18N + Clone + PartialEq + 'static>(
    children: Element,
    message: Option<Signal<Option<M>>>,
    nav: Signal<Nav<R>>,
    back_button_i18n: M,
    back_button_icon: Option<Element>,
    lang: Language,
) -> Element {
    let has_navigated = nav.with(|nav| nav.has_navigated());

    rsx! {
        if let Some(message) = message {
            p { "txt": "r",
                Banner { message, lang }
            }
        }
        div { style: "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; justify-content: center;",
            if has_navigated {
                button {
                    onclick: move |_| {
                        nav.write().go_back();
                    },
                    {back_button_icon}
                    {back_button_i18n.render(lang)}
                }
            }
            {children}
        }
    }
}
