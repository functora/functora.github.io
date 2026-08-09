#![allow(clippy::shadow_reuse)]
use crate::messages::*;
use crate::*;

#[component]
pub fn About(note: Option<String>) -> Element {
    let lang = use_lang();
    let nav = use_context::<Signal<Nav<Route>>>();
    let message = use_message();
    let rendered = use_message_markdown(Msg::AboutText);
    let qr = use_memo(|| generate_qr_code(SHARE_APP_URL).ok());
    let _ = use_effect(move || {
        if note.as_deref() == Some(SHARE_APP_ID) {
            let script = format!("document.getElementById('{SHARE_APP_ID}')?.scrollIntoView({{behavior: 'smooth'}})");
            _ = document::eval(&script);
        }
    });
    rsx! {
        Breadcrumb { title: Msg::Application }
        section {
            p { dangerous_inner_html: "{rendered()}" }
            Pre { id: SHARE_APP_ID,
                code {
                    "{Msg::AboutAndroidBeta1.render(lang)} "
                    ExtLink { href: BETA_TEST_URL, "{Msg::AboutAndroidBetaLink1.render(lang)}" }
                    " {Msg::AboutAndroidBeta2.render(lang)} "
                    ExtLink { href: GOOGLE_PLAY_URL, "{Msg::AboutAndroidBetaLink2.render(lang)}" }
                    "{Msg::AboutAndroidBeta3.render(lang)} "
                    ExtLink { href: APK_URL, "{Msg::AboutAndroidBetaLink3.render(lang)}" }
                    " {Msg::AboutAndroidBeta4.render(lang)}"
                }
            }
            if let Some(qr_image) = qr() {
                div { dangerous_inner_html: "{qr_image}" }
            }
            Dock { message,
                Button {
                    icon: Some(FaCopy),
                    primary: true,
                    onclick: move |_| {
                        write_clipboard(
                            format!("{}\n{}", Msg::ShareAppDesc.render(lang), SHARE_APP_URL),
                            message,
                        );
                    },
                    i18n: Some(Msg::CopyAppLink),
                    lang,
                }
                Button {
                    icon: Some(FaShareNodes),
                    primary: true,
                    onclick: move |_| {
                        let mut msg = message;
                        let text = Msg::ShareAppDesc.render(lang);
                        let _ = spawn(async move {
                            let data = ShareData {
                                title: "Cryptonote".into(),
                                text,
                                url: SHARE_APP_URL.into(),
                            };
                            match web_share(data).await {
                                Ok(()) => msg.set(Some(Msg::Sent)),
                                Err(e) => msg.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into()))),
                            }
                        });
                    },
                    i18n: Some(Msg::ShareAppLink),
                    lang,
                }
                ExtLink { href: BETA_TEST_URL, button: true,
                    Icon { icon: FaGoogle }
                    "{Msg::JoinTestingButton.render(lang)}"
                }
                ExtLink { href: GOOGLE_PLAY_URL, button: true,
                    Icon { icon: FaGooglePlay }
                    "{Msg::GooglePlayButton.render(lang)}"
                }
                ExtLink { href: APK_URL, button: true,
                    Icon { icon: FaAndroid }
                    "{Msg::DownloadApkButton.render(lang)}"
                }
                ExtLink { href: SOURCE_CODE_URL, button: true,
                    Icon { icon: FaGithub }
                    "{Msg::SourceCodeButton.render(lang)}"
                }
                ExtLink { href: FUNCTORA_URL, button: true,
                    Icon { icon: FaUser }
                    "{Msg::AuthorButton.render(lang)}"
                }
                NavLink {
                    nav,
                    href: Screen::Donate.to_route(None).to_string(),
                    button: true,
                    Icon { icon: FaHeart }
                    "{Msg::Donate.render(lang)}"
                }
            }
        }
    }
}
