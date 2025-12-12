use crate::Route;
use crate::i18n::Language;
use dioxus::prelude::*;

#[component]
pub fn Layout() -> Element {
    let mut language = use_context::<Signal<Language>>();
    let mut app_context =
        use_context::<Signal<crate::AppContext>>();
    let nav = use_navigator();

    let mut nav_state =
        use_context::<Signal<crate::NavigationState>>();

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    a {
                        href: "#",
                        onclick: move |evt| {
                            evt.prevent_default();
                            app_context.set(crate::AppContext::default());
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
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Route::License {});
                },
                "{crate::i18n::get_translations(language()).terms_of_service}"
            }
            " "
            {crate::i18n::get_translations(language()).you_agree}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Route::Privacy {});
                },
                "{crate::i18n::get_translations(language()).privacy_policy_and}"
            }
            ". "
            {crate::i18n::get_translations(language()).please}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Route::Donate {});
                },
                "{crate::i18n::get_translations(language()).donate_link}"
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
