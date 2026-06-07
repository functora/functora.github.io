use crate::messages::*;
use crate::*;
use functora_dioxus::use_nav;

#[component]
pub fn Layout() -> Element {
    let mut cfg = use_context::<Signal<AppCfg>>();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let lang = use_lang();
    let nav = use_nav::<Route>();
    let nav_signal =
        use_context_provider(|| Signal::new(nav));

    use_effect(move || {
        let theme = cfg.read().theme;
        spawn(async move {
            if let Err(e) =
                functora_dioxus::js::js_set_theme(&theme)
                    .await
            {
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
                        nav: nav_signal,
                        href: Screen::Home.to_route(None).to_string(),
                        onclick: move |_| ctx.set(AppCtx::default()),
                        "🔐 Cryptonote"
                    }
                }
                ul {
                    for lang in SUPPORTED_LANGUAGES {
                        li {
                            a {
                                onclick: move |_| cfg.write().language = *lang,
                                "{language_label(*lang)}"
                            }
                        }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                cfg.with_mut(|x| {
                                    x.theme = x.theme.next();
                                });
                            },
                            {match cfg.read().theme {
                                Theme::Light => "🌝 ",
                                Theme::Dark => "🌚 ",
                            }}
                            {MsgTheme.render(lang)}
                        }
                    }
                    li {
                        NavLink { nav: nav_signal, href: Screen::About.to_route(None).to_string(), "❓{MsgAboutTitle.render(lang)}" }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {MsgCopyright.render(lang)}
            " 2025 "
            ExtLink { href: FUNCTORA_URL, "Functora" }
            ". "
            {MsgAllRightsReserved.render(lang)}
            " "
            {MsgByContinuing.render(lang)}
            " "
            NavLink { nav: nav_signal, href: Screen::License.to_route(None).to_string(), "{MsgTermsOfService.render(lang)}" }
            " "
            {MsgYouAgree.render(lang)}
            " "
            NavLink { nav: nav_signal, href: Screen::Privacy.to_route(None).to_string(), "{MsgPrivacyPolicyAnd.render(lang)}" }
            ". "
            {MsgPlease.render(lang)}
            " "
            NavLink { nav: nav_signal, href: Screen::Donate.to_route(None).to_string(), "{MsgDonateLink.render(lang)}" }
            ". "
            {MsgVersionLabel.render(lang)}
            " "
            {APP_VERSION}
            "."
        }

        br {}
    }
}
