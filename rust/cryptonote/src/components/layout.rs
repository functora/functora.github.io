use crate::*;

#[component]
pub fn Layout() -> Element {
    let mut cfg = use_context::<Signal<AppCfg>>();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let t = use_translations();

    use_effect(move || {
        let theme = cfg.read().theme;
        spawn(async move {
            if let Err(e) = js_set_theme(&theme).await {
                tracing::error!(
                    "Set theme error: {:#?}",
                    e
                );
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
                        onclick: move |_| ctx.set(AppCtx::default()),
                        "🔐 Cryptonote"
                    }
                }

                ul {
                    li {
                        a { onclick: move |_| cfg.write().language = Language::English,
                            "🇬🇧 English"
                        }
                    }
                    li {
                        a { onclick: move |_| cfg.write().language = Language::Spanish,
                            "🇪🇸 Español"
                        }
                    }
                    li {
                        a { onclick: move |_| cfg.write().language = Language::Russian,
                            "🇷🇺 Русский"
                        }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                cfg.with_mut(|x| {
                                    x.theme = next_cycle(&x.theme);
                                });
                            },
                            {
                                match cfg.read().theme {
                                    Theme::Light => "🌝 ",
                                    Theme::Dark => "🌚 ",
                                }
                            }
                            {t.theme}
                        }
                    }
                    li {
                        NavLink { route: Screen::About.to_route(None), "❓{t.about_title}" }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {t.copyright}
            " 2025 "
            ExtLink { href: FUNCTORA_URL, "Functora" }
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
            {APP_VERSION}
            "."
        }

        br {}
    }
}
