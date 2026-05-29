use crate::traits::NavCtx;
use crate::widgets::message::Message;
use dioxus::prelude::ReadableExt;
use dioxus::prelude::*;

#[component]
pub fn Dock<N: NavCtx + PartialEq>(
    children: Element,
    message: Option<Signal<Option<String>>>,
    nav_ctx: Signal<N>,
    back_button_text: String,
    back_button_icon: Option<Element>,
) -> Element {
    let has_navigated = nav_ctx.with(super::super::traits::NavCtx::can_go_back);

    rsx! {
        if let Some(message) = message {
            p { "txt": "r",
                Message { message }
            }
        }
        div { style: "display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; justify-content: center;",
            if has_navigated {
                button {
                    onclick: move |_| {
                        nav_ctx.write().go_back();
                    },
                    {back_button_icon}
                    {back_button_text}
                }
            }
            {children}
        }
    }
}
