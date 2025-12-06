use crate::Route;
use crate::i18n::Language;
use dioxus::prelude::*;
use dioxus_router::prelude::navigator;

#[component]
pub fn Navbar() -> Element {
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
                                .set(crate::AppContext {
                                    content: None,
                                    password: String::new(),
                                    cipher: None,
                                    share_url: None,
                                    qr_code: None,
                                });
                            nav.push("/");
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

        footer {
            a {
                href: "/license",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav.push("/license");
                },
                "{crate::i18n::get_translations(language()).license}"
            }
            " | "
            a {
                href: "/privacy",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav.push("/privacy");
                },
                "{crate::i18n::get_translations(language()).privacy}"
            }
        }
    }
}
