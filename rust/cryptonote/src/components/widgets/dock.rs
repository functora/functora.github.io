use crate::messages::*;
use crate::*;

#[component]
pub fn Dock(
    children: Element,
    message: Option<Signal<Option<UiMessage>>>,
) -> Element {
    let mut nav = use_app_nav();
    let lang = use_lang();

    rsx! {
        if let Some(message) = message {
            p { "txt": "r",
                Message { message }
            }
        }
        div { style: "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; justify-content: center;",
            if nav.with(|n| n.has_navigated()) {
                Button {
                    icon: FaArrowLeft,
                    onclick: move |_| {
                        nav.write().go_back();
                    },
                    "{MsgBack.render(lang)}"
                }
            }
            {children}
        }
    }
}
