use crate::i18n::{Language, I18N};
use dioxus::prelude::*;
use dioxus_free_icons::{Icon, IconShape};

#[component]
pub fn Button<T: IconShape + Clone + PartialEq + 'static, U: I18N + Clone + PartialEq + 'static>(
    children: Element,
    onclick: Option<EventHandler<MouseEvent>>,
    #[props(extends = button, extends = GlobalAttributes)] attributes: Vec<Attribute>,
    #[props(!optional)] icon: Option<T>,
    #[props(!optional)] i18n: Option<U>,
    #[props(default)] lang: Language,
    #[props(default)] primary: bool,
) -> Element {
    rsx! {
        button {
            onclick: move |evt| {
                if let Some(handler) = &onclick {
                    handler.call(evt);
                }
            },
            "primary": if primary { Some("") } else { None },
            ..attributes,
            {icon.map(|i| rsx! {
                Icon { icon: i }
                " "
            })}
            {i18n.map(|i| rsx! {
                {i.render(lang)}
            })}
            {children}
        }
    }
}
