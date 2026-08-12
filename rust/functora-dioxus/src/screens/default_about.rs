#![allow(clippy::shadow_reuse)]
use crate::encoding::generate_qr_code;
use crate::ffi::{ShareData, social_share};
use crate::hooks::{use_lang, use_message, use_message_markdown};
use crate::i18n::I18N;
use crate::messages::Msg;
use crate::nav::Nav;
use crate::white_label::AppContent;
use crate::widgets::{Breadcrumb, Button, Dock, ExtLink, NavLink, Pre};
use crate::write_clipboard;
use dioxus::prelude::*;
use dioxus_free_icons::Icon;
use dioxus_free_icons::icons::fa_brands_icons::{FaAndroid, FaGithub, FaGoogle, FaGooglePlay};
use dioxus_free_icons::icons::fa_solid_icons::{FaCopy, FaHeart, FaShareNodes, FaUser};

#[component]
pub fn DefaultAbout<R, M, N>(
    home_route: R,
    nav: N,
    content: AppContent<R, M>,
    #[props(default)] note: Option<String>,
) -> Element
where
    R: Routable + Clone + Default + PartialEq + 'static,
    M: I18N + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
{
    let lang = use_lang();
    let message = use_message::<Msg>();
    let AppContent {
        attrs,
        donate,
        about_text,
        share_desc,
    } = content;
    let rendered = use_message_markdown(about_text);
    let share_text = share_desc.render(lang);
    let derived_app_url = attrs.app_url();
    let derived_app_name = attrs.app_name();
    let derived_apk_url = attrs.apk_url();
    let derived_google_play_url = attrs.google_play_url();
    let derived_beta_url = attrs.beta_url();
    let source_url = attrs.source_url();
    let author_url = attrs.author_url();
    let anchor_id = attrs.share_anchor_id();
    #[cfg(feature = "qr")]
    let qr = use_memo({
        let url = derived_app_url.clone();
        move || generate_qr_code(&url).ok()
    });
    #[cfg(not(feature = "qr"))]
    let qr = use_memo(|| None);
    let _ = use_effect({
        let anchor_id = anchor_id.clone();
        let note = note.clone();
        move || {
            if note.as_deref() == Some(anchor_id.as_str()) {
                let _ = document::eval(&format!(
                    "document.getElementById('{anchor_id}')?.scrollIntoView({{behavior: 'smooth'}})"
                ));
            }
        }
    });
    let copy_text = format!("{}\n{}", share_text.clone(), derived_app_url.clone());
    let share_text_owned = share_text;
    let share_url_owned = derived_app_url;
    rsx! {
        Breadcrumb {
            title: Msg::Application,
            home_label: Msg::Home,
            home_route,
            nav: nav.clone(),
            lang,
        }
        section {
            p { dangerous_inner_html: "{rendered()}" }
            Pre {
                id: anchor_id,
                code {
                    "{Msg::AboutAndroidBeta1.render(lang)} "
                    ExtLink { href: derived_beta_url.clone(), "{Msg::AboutAndroidBetaLink1.render(lang)}" }
                    " {Msg::AboutAndroidBeta2.render(lang)} "
                    ExtLink { href: derived_google_play_url.clone(), "{Msg::AboutAndroidBetaLink2.render(lang)}" }
                    "{Msg::AboutAndroidBeta3.render(lang)} "
                    ExtLink { href: derived_apk_url.clone(), "{Msg::AboutAndroidBetaLink3.render(lang)}" }
                    " {Msg::AboutAndroidBeta4.render(lang)}"
                }
            }
            if let Some(qr_image) = qr() {
                div { dangerous_inner_html: "{qr_image}" }
            }
            Dock {
                nav: nav.clone(),
                message: Some(message),
                lang,
                Button {
                    icon: Some(FaCopy),
                    primary: true,
                    onclick: move |_| {
                        write_clipboard(copy_text.clone(), message, Msg::Copied, |e| {
                            Msg::ClipboardWriteError(e.to_string())
                        });
                    },
                    i18n: Some(Msg::CopyAppLink),
                    lang,
                }
                Button {
                    icon: Some(FaShareNodes),
                    primary: true,
                    onclick: move |_| {
                        let mut msg = message;
                        let title = derived_app_name.clone();
                        let text = share_text_owned.clone();
                        let url = share_url_owned.clone();
                        let _ = spawn(async move {
                            let data = ShareData { title, text, url };
                            match social_share(data).await {
                                Ok(()) => msg.set(Some(Msg::Sent)),
                                Err(e) => msg.set(Some(Msg::ErrorTitle(e.to_string()))),
                            }
                        });
                    },
                    i18n: Some(Msg::ShareAppLink),
                    lang,
                }
                ExtLink {
                    href: derived_beta_url,
                    button: true,
                    Icon { icon: FaGoogle }
                    "{Msg::JoinTestingButton.render(lang)}"
                }
                ExtLink {
                    href: derived_google_play_url,
                    button: true,
                    Icon { icon: FaGooglePlay }
                    "{Msg::GooglePlayButton.render(lang)}"
                }
                ExtLink {
                    href: derived_apk_url,
                    button: true,
                    Icon { icon: FaAndroid }
                    "{Msg::DownloadApkButton.render(lang)}"
                }
                ExtLink {
                    href: source_url,
                    button: true,
                    Icon { icon: FaGithub }
                    "{Msg::SourceCodeButton.render(lang)}"
                }
                ExtLink {
                    href: author_url,
                    button: true,
                    Icon { icon: FaUser }
                    "{Msg::AuthorButton.render(lang)}"
                }
                NavLink {
                    nav: nav.clone(),
                    href: donate.to_string(),
                    button: true,
                    Icon { icon: FaHeart }
                    "{Msg::Donate.render(lang)}"
                }
            }
        }
    }
}
