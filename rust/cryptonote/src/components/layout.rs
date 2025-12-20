use crate::*;

#[component]
pub fn Layout() -> Element {
    let mut app_settings =
        use_context::<Signal<AppSettings>>();
    let mut app_context =
        use_context::<Signal<AppContext>>();

    let t = get_translations(app_settings.read().language);

    use_effect(move || {
        let theme = app_settings.read().theme;
        spawn(async move {
            if let Err(e) = js_set_theme(&theme).await {
                tracing::error!("{:#?}", e);
            }
        });
    });

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    NavLink {
                        route: Screen::Home.to_route(None),
                        onclick: move |_| app_context.set(AppContext::default()),
                        "🔐 Cryptonote"
                    }
                }

                ul {
                    li {
                        a { onclick: move |_| app_settings.write().language = Language::English,
                            "🇬🇧 English"
                        }
                    }
                    li {
                        a { onclick: move |_| app_settings.write().language = Language::Spanish,
                            "🇪🇸 Español"
                        }
                    }
                    li {
                        a { onclick: move |_| app_settings.write().language = Language::Russian,
                            "🇷🇺 Русский"
                        }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                let mut settings = app_settings.write();
                                let prev = settings.theme;
                                settings.theme = next_cycle(&prev);
                            },
                            {
                                match app_settings.read().theme {
                                    Theme::Light => "🌚 ",
                                    Theme::Dark => "🌝 ",
                                }
                            }
                            {t.theme}
                        }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {t.copyright}
            " 2025 "
            a { href: "https://functora.github.io/", "Functora" }
            ". "
            {t.all_rights_reserved}
            " "
            {t.by_continuing}
            " "
            NavLink { route: Screen::License.to_route(None), "{t.terms_of_service}" }
            " "
            {t.you_agree}
            " "
            NavLink { route: Screen::Privacy.to_route(None), "{t.privacy_policy_and}" }
            ". "
            {t.please}
            " "
            NavLink { route: Screen::Donate.to_route(None), "{t.donate_link}" }
            ". "
            {t.version_label}
            " "
            {env!("CARGO_PKG_VERSION")}
            "."
        }

        br {}
    }
}
