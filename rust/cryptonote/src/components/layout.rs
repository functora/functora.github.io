use crate::messages::*;
use crate::*;

#[component]
pub fn Layout() -> Element {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    let mut tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let idx = use_signal(|| 0u32);
    let nav = use_nav::<Route, _>(idx.into());
    let nav_signal = use_context_provider(|| Signal::new(nav));

    use_effect(move || {
        let theme = pst.theme()();
        spawn(async move {
            if let Err(e) = functora_dioxus::ffi::set_theme(&theme).await {
                tracing::error!("Set theme error: {:#?}", e);
            }
        });
    });

    rsx! {
        nav {
            label {
                input { r#type: "checkbox", id: "functora-nav-toggle" }
                header {
                    NavLink {
                        nav: nav_signal,
                        href: Screen::Home.to_route(None).to_string(),
                        onclick: move |_| tst.set(TemporaryState::default()),
                        "🔐 Cryptonote"
                    }
                    span { id: "functora-nav-open" }
                }
                span { id: "functora-nav-close" }
                ul {
                    for lang in SUPPORTED_LANGUAGES {
                        li {
                            a {
                                onclick: move |_| {
                                    pst.language().set(*lang);
                                },
                                "{language_label(*lang)}"
                            }
                        }
                    }
                    li {
                        NavLink {
                            nav: nav_signal,
                            href: Screen::About.to_route(None).to_string(),
                            "❓{Msg::AboutTitle.render(lang)}"
                        }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                pst.theme().with_mut(|t| *t = t.next());
                            },
                            {
                                match pst.theme()() {
                                    Theme::Light => "🌝 ",
                                    Theme::Dark => "🌚 ",
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
                {Msg::Please.render(lang)}
                " "
                NavLink {
                    nav: nav_signal,
                    href: Screen::Donate.to_route(None).to_string(),
                    "{Msg::DonateLink.render(lang)}"
                }
                ". "
                {Msg::VersionLabel.render(lang)}
                " "
                {APP_VERSION}
                "."
            }
        }
    }
}
