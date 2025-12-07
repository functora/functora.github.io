use crate::Route;
use crate::i18n::Language;
use dioxus::prelude::*;
use dioxus_router::prelude::navigator;

#[component]
pub fn Layout() -> Element {
    let mut language = use_context::<Signal<Language>>();
    let mut app_context =
        use_context::<Signal<crate::AppContext>>();
    let nav = navigator();

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    a {
                        href: "#",
                        onclick: move |_| {
                            app_context
                                .set(crate::AppContext::default());
                            nav.push(Route::Home {});
                        },
                        "🔐 Cryptonote"
                    }
                }
                ul {
                    li {
                        a { onclick: move |_| language.set(Language::English), "English" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Spanish), "Español" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Russian), "Русский" }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {crate::i18n::get_translations(language()).copyright}
            " 2025 "
            a { href: "https://functora.github.io/", "Functora" }
            ". "
            {crate::i18n::get_translations(language()).all_rights_reserved}
            " "
            {crate::i18n::get_translations(language()).by_continuing}
            " "
            Link { to: Route::License {},
                "{crate::i18n::get_translations(language()).terms_of_service}"
            }
            " "
            {crate::i18n::get_translations(language()).you_agree}
            " "
            Link { to: Route::Privacy {},
                "{crate::i18n::get_translations(language()).privacy_policy_and}"
            }
            ". "
            {crate::i18n::get_translations(language()).version_label}
            " "
            {env!("CARGO_PKG_VERSION")}
            "."
        }

        br {}
    }
}
