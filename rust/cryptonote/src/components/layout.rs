use crate::messages::*;
use crate::*;

#[component]
pub fn Layout() -> Element {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let idx = use_signal(|| 0u32);
    let nav = use_nav::<Route, _>(idx.into());
    let mut dl_nav = nav.clone();
    let nav_signal = use_context_provider(|| Signal::new(nav));
    let message = use_message();

    use_hook(|| {
        crate::deep_link::set_schedule_update(dioxus::core::schedule_update());
    });

    dioxus::core::use_after_render(move || {
        if let Some(source) = crate::deep_link::take_archive() {
            let mut message_out = message;
            let _ = spawn(async move {
                if let Err(e) = crate::hooks::open_archive_async(source, tst, nav_signal).await {
                    message_out.set(Some(Msg::Error(e)));
                }
            });
            return;
        }
        if let Some(route) = crate::deep_link::take_url().and_then(|url| crate::deep_link::url_to_route(&url)) {
            dl_nav.push_route(&route);
        }
    });

    let _ = use_effect(move || {
        let _ = idx();
        let _ = document::eval("window.scrollTo(0, 0)");
    });

    let _ = use_effect(move || {
        let theme = pst.theme()();
        let _ = spawn(async move {
            if let Err(e) = functora_dioxus::ffi::set_theme(&theme).await {
                tracing::error!("Set theme error: {:#?}", e);
            }
        });
    });

    rsx! {
        nav { "fx": "",
            label {
                input { r#type: "checkbox", id: "functora-nav-toggle" }
                header {
                    NavLink {
                        nav: nav_signal,
                        href: Screen::Home.to_route(None).to_string(),
                        onclick: move |_| {
                            collapse_nav();
                            reset_temporary_state(tst);
                        },
                        "🔐 Cryptonote"
                    }
                    span { id: "functora-nav-open" }
                }
                span { id: "functora-nav-collapse" }
                ul {
                    for supported_lang in SUPPORTED_LANGUAGES.iter().copied() {
                        li {
                            a {
                                onclick: move |evt| {
                                    evt.prevent_default();
                                    collapse_nav();
                                    pst.language().set(supported_lang);
                                },
                                span { "{Msg::LanguageFlag(supported_lang).render(supported_lang)}" }
                                "{Msg::LanguageName(supported_lang).render(supported_lang)}"
                            }
                        }
                    }
                    li {
                        NavLink {
                            nav: nav_signal,
                            href: Screen::About.to_route(None).to_string(),
                            onclick: move |_| collapse_nav(),
                            Icon { icon: FaAndroid }
                            "{Msg::Application.render(lang)}"
                        }
                    }
                    li {
                        a {
                            onclick: move |evt| {
                                evt.prevent_default();
                                collapse_nav();
                                pst.theme().with_mut(|t| *t = t.next());
                            },
                            span {
                                match pst.theme()() {
                                    Theme::Light => "☀️",
                                    Theme::Dark => "🌙",
                                }
                            }
                            {Msg::Theme.render(lang)}
                        }
                    }
                }
            }
        }

        Outlet::<Route> {}

        footer {
            p {
                {Msg::Copyright.render(lang)}
                " 2025 "
                ExtLink { href: FUNCTORA_URL, "Functora" }
                ". "
                {Msg::AllRightsReserved.render(lang)}
                " "
                {Msg::ByContinuing.render(lang)}
                " "
                NavLink {
                    nav: nav_signal,
                    href: Screen::License.to_route(None).to_string(),
                    "{Msg::TermsOfService.render(lang)}"
                }
                " "
                {Msg::YouAgree.render(lang)}
                " "
                NavLink {
                    nav: nav_signal,
                    href: Screen::Privacy.to_route(None).to_string(),
                    "{Msg::PrivacyPolicyAnd.render(lang)}"
                }
                ". "
                NavLink {
                    nav: nav_signal,
                    href: Screen::Donate.to_route(None).to_string(),
                    "{Msg::DonateLink.render(lang)}"
                }
                " "
                {Msg::And.render(lang)}
                " "
                NavLink {
                    nav: nav_signal,
                    href: Screen::About.to_route(Some(SHARE_APP_ID.into())).to_string(),
                    "{Msg::FooterShareWord.render(lang)}"
                }
                " {Msg::FooterAppWord.render(lang)}"
                ". "
                {Msg::VersionLabel.render(lang)}
                " "
                {APP_VERSION}
                "."
            }
        }

        ProgressBar {}
    }
}

fn collapse_nav() {
    _ = document::eval("document.getElementById('functora-nav-toggle').checked = false");
}
