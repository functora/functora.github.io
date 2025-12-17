use crate::*;

#[component]
pub fn Layout() -> Element {
    let mut theme = use_signal(|| Theme::Light);
    let mut language = use_context::<Signal<Language>>();
    let mut app_context =
        use_context::<Signal<AppContext>>();
    let nav = use_navigator();

    let mut nav_state =
        use_context::<Signal<NavigationState>>();

    use_effect(move || {
        let theme = *theme.read();
        spawn(async move {
            if let Err(e) = js_data_theme(&theme).await {
                tracing::error!("{:#?}", e);
            }
        });
    });

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    a {
                        href: "#",
                        onclick: move |evt| {
                            evt.prevent_default();
                            app_context.set(AppContext::default());
                            nav.push(Screen::Home.to_route(None));
                        },
                        "🔐 Cryptonote"
                    }
                }

                ul {
                    li {
                        a { onclick: move |_| language.set(Language::English), "🇬🇧 English" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Spanish), "🇪🇸 Español" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Russian),
                            "🇷🇺 Русский"
                        }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                let prev = *theme.read();
                                theme.set(next_cycle(&prev))
                            },
                            {
                                match *theme.read() {
                                    Theme::Light => "🌚 ",
                                    Theme::Dark => "🌝 ",
                                }
                            }
                            {get_translations(language()).theme}
                        }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {get_translations(language()).copyright}
            " 2025 "
            a { href: "https://functora.github.io/", "Functora" }
            ". "
            {get_translations(language()).all_rights_reserved}
            " "
            {get_translations(language()).by_continuing}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::License.to_route(None));
                },
                "{get_translations(language()).terms_of_service}"
            }
            " "
            {get_translations(language()).you_agree}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::Privacy.to_route(None));
                },
                "{get_translations(language()).privacy_policy_and}"
            }
            ". "
            {get_translations(language()).please}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::Donate.to_route(None));
                },
                "{get_translations(language()).donate_link}"
            }
            ". "
            {get_translations(language()).version_label}
            " "
            {env!("CARGO_PKG_VERSION")}
            "."
        }

        br {}
    }
}
