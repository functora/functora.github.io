use crate::Route;
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;

#[component]
pub fn Navbar() -> Element {
    let mut language = use_context::<Signal<Language>>();
    let _t = get_translations(language());

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    Link { to: Route::Home {}, "🔐 Cryptonote" }
                }
                ul {
                    li { class: if language() == Language::English { "selected" },
                        a { onclick: move |_| language.set(Language::English), "English" }
                    }
                    li { class: if language() == Language::Spanish { "selected" },
                        a { onclick: move |_| language.set(Language::Spanish), "Español" }
                    }
                    li { class: if language() == Language::Russian { "selected" },
                        a { onclick: move |_| language.set(Language::Russian), "Русский" }
                    }
                }
            }
        }

        Outlet::<Route> {}
    }
}
