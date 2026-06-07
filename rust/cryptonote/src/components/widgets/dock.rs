use crate::messages::*;
use crate::*;

#[component]
pub fn Dock(
    children: Element,
    message: Option<Signal<Option<UiMessage>>>,
) -> Element {
    let nav = use_app_nav();
    let lang = use_lang();

    rsx! {
        if let Some(message) = message {
            p { "txt": "r",
                Message { message }
            }
        }
        div { style: "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; justify-content: center;",
            if nav.has_navigated() {
                Button {
                    icon: FaArrowLeft,
                    onclick: move |_| {
                        nav.go_back();
                    },
                    "{MsgBack.render(lang)}"
                }
            }
            {children}
        }
    }
}
