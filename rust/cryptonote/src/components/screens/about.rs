use crate::messages::*;
use crate::*;

#[component]
pub fn About() -> Element {
    let lang = use_lang();
    let nav = use_context::<Signal<Nav<Route>>>();
    rsx! {
        Breadcrumb { title: Msg::AboutTitle }
        section {
            Pre { "{Msg::AboutText.render(lang)}" }
            p {
                "{Msg::AboutAndroidBeta1.render(lang)} "
                ExtLink { href: BETA_TEST_URL, "{Msg::AboutAndroidBetaLink1.render(lang)}" }
                " {Msg::AboutAndroidBeta2.render(lang)} "
                ExtLink { href: GOOGLE_PLAY_URL, "{Msg::AboutAndroidBetaLink2.render(lang)}" }
                "{Msg::AboutAndroidBeta3.render(lang)} "
                ExtLink { href: APK_URL, "{Msg::AboutAndroidBetaLink3.render(lang)}" }
                " {Msg::AboutAndroidBeta4.render(lang)}"
            }
            Dock {
                ExtLink { href: BETA_TEST_URL, button: true, primary: true,
                    Icon { icon: FaGoogle }
                    "{Msg::JoinTestingButton.render(lang)}"
                }
                ExtLink { href: GOOGLE_PLAY_URL, button: true, primary: true,
                    Icon { icon: FaGooglePlay }
                    "{Msg::GooglePlayButton.render(lang)}"
                }
                ExtLink { href: APK_URL, button: true, primary: true,
                    Icon { icon: FaAndroid }
                    "{Msg::DownloadApkButton.render(lang)}"
                }
                ExtLink { href: SOURCE_CODE_URL, button: true, primary: true,
                    Icon { icon: FaGithub }
                    "{Msg::SourceCodeButton.render(lang)}"
                }
                ExtLink { href: FUNCTORA_URL, button: true, primary: true,
                    Icon { icon: FaUser }
                    "{Msg::AuthorButton.render(lang)}"
                }
                NavLink {
                    nav,
                    href: Screen::Donate.to_route(None).to_string(),
                    button: true,
                    primary: true,
                    Icon { icon: FaHeart }
                    "{Msg::DonateButton.render(lang)}"
                }
            }
        }
    }
}
