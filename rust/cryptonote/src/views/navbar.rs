use crate::Route;
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;

#[component]
pub fn Navbar() -> Element {
    let mut language = use_context::<Signal<Language>>();
    let t = get_translations(language());

    rsx! {
        nav {
            Link { to: Route::Home {}, "🔐 {t.app_title}" }
            select {
                value: "{language().code()}",
                onchange: move |evt| {
                    language
                        .set(
                            match evt.value().as_str() {
                                "es" => Language::Spanish,
                                "ru" => Language::Russian,
                                _ => Language::English,
                            },
                        );
                },
                option { value: "en", "English" }
                option { value: "es", "Español" }
                option { value: "ru", "Русский" }
            }
        }

        Outlet::<Route> {}
    }
}
