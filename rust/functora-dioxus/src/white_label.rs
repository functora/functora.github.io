#![allow(clippy::shadow_reuse)]
use crate::ffi::Theme;
use crate::hooks::use_lang;
use crate::i18n::{I18N, SUPPORTED_LANGUAGES};
use crate::messages::Msg;
use crate::nav::use_nav;
use crate::state::{PersistentState, PersistentStateStoreExt};
use crate::storage::PersistentSignal;
use crate::widgets::{ExtLink, NavLink};
use dioxus::prelude::*;
use dioxus_free_icons::icons::fa_brands_icons::FaAndroid;
use dioxus_free_icons::{Icon, IconShape};

#[derive(Clone, Debug, PartialEq)]
pub struct CryptoBlock {
    pub label: String,
    pub address: String,
}

const BTC_ADDRESS: &str = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str =
    "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

#[must_use]
pub fn donate_blocks() -> Vec<CryptoBlock> {
    vec![
        CryptoBlock {
            label: "BTC - Bitcoin".to_string(),
            address: BTC_ADDRESS.to_string(),
        },
        CryptoBlock {
            label: "XMR - Monero".to_string(),
            address: XMR_ADDRESS.to_string(),
        },
    ]
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhiteLabelContent<M = Msg> {
    pub license_text: Option<M>,
    pub privacy_text: Option<M>,
    pub donate_greeting: Option<M>,
    pub donate_intro: Option<M>,
    pub donate_blocks: Vec<CryptoBlock>,
}

impl<M> Default for WhiteLabelContent<M> {
    fn default() -> Self {
        Self {
            license_text: None,
            privacy_text: None,
            donate_greeting: None,
            donate_intro: None,
            donate_blocks: donate_blocks(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AppAttrs {
    pub app: &'static str,
    pub vsn: &'static str,
    pub org: &'static str,
    pub src: Option<&'static str>,
    pub dst: &'static str,
}

impl AppAttrs {
    #[must_use]
    pub fn app_name(self) -> String {
        let end = self.app.chars().next().map_or(0, char::len_utf8);
        format!("{}{}", self.app[..end].to_uppercase(), &self.app[end..])
    }

    #[must_use]
    pub fn pages_url(self) -> String {
        format!("https://{}.github.io", self.org)
    }

    #[must_use]
    pub fn author_url(self) -> String {
        format!("{}/", self.pages_url())
    }

    #[must_use]
    pub fn app_url(self) -> String {
        format!("https://{}.github.io/{}/{}", self.org, self.dst, self.app)
    }

    #[must_use]
    pub fn source_url(self) -> String {
        self.src.map_or_else(
            || format!("https://github.com/{}/{}.github.io", self.org, self.org),
            |src| {
                format!(
                    "https://github.com/{}/{}.github.io/tree/master/{src}/{}",
                    self.org, self.org, self.app
                )
            },
        )
    }

    #[must_use]
    pub fn apk_url(self) -> String {
        format!(
            "https://github.com/{}/{}.github.io/releases/tag/{}-v{}",
            self.org, self.org, self.app, self.vsn
        )
    }

    #[must_use]
    pub fn google_play_url(self) -> String {
        format!(
            "https://play.google.com/store/apps/details?id=com.{}.{}",
            self.org, self.app
        )
    }

    #[must_use]
    pub fn beta_url(self) -> String {
        format!("https://groups.google.com/g/{}", self.org)
    }

    #[must_use]
    pub fn share_anchor_id(self) -> String {
        let hash = self
            .app
            .bytes()
            .fold(0u32, |acc, byte| acc.wrapping_mul(31).wrapping_add(u32::from(byte)));
        format!("sh-{hash:08x}")
    }
}

#[derive(Clone, PartialEq)]
pub struct AppContent<R, M = Msg> {
    pub attrs: AppAttrs,
    pub donate: R,
    pub about_text: M,
    pub share_desc: M,
}

#[derive(Clone, PartialEq)]
pub struct WhiteLabelConfig<R, I = FaAndroid> {
    pub brand: String,
    pub copyright_owner: String,
    pub copyright_owner_href: Option<String>,
    pub version: Option<String>,
    pub home: R,
    pub about: Option<R>,
    pub about_icon: Option<I>,
    pub donate: Option<R>,
    pub license: Option<R>,
    pub privacy: Option<R>,
    pub share: Option<R>,
    pub on_brand_click: Option<EventHandler<MouseEvent>>,
    pub bottom_extra: Element,
}

#[component]
pub fn WhiteLabelLayout<
    R: Routable + Clone + Default + PartialEq + 'static,
    I: IconShape + Clone + PartialEq + 'static,
>(
    config: WhiteLabelConfig<R, I>,
    children: Element,
) -> Element {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    let lang = use_lang();
    let idx = use_signal(|| 0u32);
    let nav = use_nav::<R, _>(idx.into());
    let nav_signal = use_context_provider(|| Signal::new(nav));

    let _ = use_effect(move || {
        let _ = idx();
        let _ = document::eval("window.scrollTo(0, 0)");
    });

    let _ = use_effect(move || {
        let theme = pst.theme()();
        let _ = spawn(async move {
            if let Err(e) = crate::ffi::set_theme(&theme).await {
                tracing::error!("Set theme error: {:#?}", e);
            }
        });
    });

    let WhiteLabelConfig {
        brand,
        copyright_owner,
        copyright_owner_href,
        version,
        home,
        about,
        about_icon,
        donate: donate_route,
        license: license_route,
        privacy: privacy_route,
        share: share_route,
        on_brand_click,
        bottom_extra,
    } = config;

    let owner = if let Some(href) = &copyright_owner_href {
        rsx! {
            ExtLink { href: href.clone(), "{copyright_owner}" }
        }
    } else {
        rsx! {
            "{copyright_owner}"
        }
    };

    let legal = match (&license_route, &privacy_route) {
        (Some(license), Some(privacy)) => rsx! {
            NavLink {
                nav: nav_signal,
                href: license.to_string(),
                "{Msg::TermsOfService.render(lang)}"
            }
            " "
            {Msg::YouAgree.render(lang)}
            " "
            NavLink {
                nav: nav_signal,
                href: privacy.to_string(),
                "{Msg::PrivacyPolicyAnd.render(lang)}"
            }
        },
        (Some(license), None) => rsx! {
            NavLink {
                nav: nav_signal,
                href: license.to_string(),
                "{Msg::TermsOfService.render(lang)}"
            }
        },
        (None, Some(privacy)) => rsx! {
            NavLink {
                nav: nav_signal,
                href: privacy.to_string(),
                "{Msg::PrivacyPolicyAnd.render(lang)}"
            }
        },
        (None, None) => rsx! {},
    };

    let donate_share = match (&donate_route, &share_route) {
        (Some(donate), Some(share)) => rsx! {
            NavLink {
                nav: nav_signal,
                href: donate.to_string(),
                "{Msg::DonateLink.render(lang)}"
            }
            " "
            {Msg::And.render(lang)}
            " "
            NavLink {
                nav: nav_signal,
                href: share.to_string(),
                "{Msg::FooterShareWord.render(lang)}"
            }
            " {Msg::FooterAppWord.render(lang)}"
        },
        (Some(donate), None) => rsx! {
            NavLink {
                nav: nav_signal,
                href: donate.to_string(),
                "{Msg::DonateLink.render(lang)}"
            }
        },
        (None, Some(share)) => rsx! {
            NavLink {
                nav: nav_signal,
                href: share.to_string(),
                "{Msg::FooterShareWord.render(lang)}"
            }
            " {Msg::FooterAppWord.render(lang)}"
        },
        (None, None) => rsx! {},
    };

    let version_frag = version.map_or(rsx! {}, |v| {
        rsx! {
            ". "
            {Msg::VersionLabel.render(lang)}
            " {v}."
        }
    });

    rsx! {
        nav { "fx": "",
            label {
                input { r#type: "checkbox", id: "functora-nav-toggle" }
                header {
                    NavLink {
                        nav: nav_signal,
                        href: home.to_string(),
                        onclick: move |evt| {
                            collapse_nav();
                            if let Some(f) = &on_brand_click {
                                f.call(evt);
                            }
                        },
                        "{brand}"
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
                    if let Some(route) = &about {
                        li {
                            NavLink {
                                nav: nav_signal,
                                href: route.to_string(),
                                onclick: move |_| collapse_nav(),
                                if let Some(icon) = &about_icon {
                                    Icon { icon: icon.clone() }
                                }
                                "{Msg::Application.render(lang)}"
                            }
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

        {children}

        footer {
            p {
                {Msg::Copyright.render(lang)}
                " {crate::FUNCTORA_DIOXUS_YEAR} "
                {owner}
                ". "
                {Msg::AllRightsReserved.render(lang)}
                " "
                {Msg::ByContinuing.render(lang)}
                " "
                {legal}
                ". "
                {donate_share}
                {version_frag}
            }
        }

        {bottom_extra}
    }
}

fn collapse_nav() {
    _ = document::eval("document.getElementById('functora-nav-toggle').checked = false");
}
