use crate::messages::*;
use crate::*;

#[component]
pub fn About() -> Element {
    let lang = use_lang();
    let nav = use_app_nav();
    rsx! {
        Breadcrumb { title: MsgAboutTitle }
        section {
            Pre { "{MsgAboutText.render(lang)}" }
            br {}
            br {}
            p {
                "{MsgAboutAndroidBeta1.render(lang)} "
                ExtLink { href: BETA_TEST_URL, "{MsgAboutAndroidBetaLink1.render(lang)}" }
                " {MsgAboutAndroidBeta2.render(lang)} "
                ExtLink { href: GOOGLE_PLAY_URL, "{MsgAboutAndroidBetaLink2.render(lang)}" }
                "{MsgAboutAndroidBeta3.render(lang)} "
                ExtLink { href: APK_URL, "{MsgAboutAndroidBetaLink3.render(lang)}" }
                " {MsgAboutAndroidBeta4.render(lang)}"
            }
            Dock {
                ExtLink { href: BETA_TEST_URL, button: true, primary: true,
                    Icon { icon: FaGoogle }
                    "{MsgJoinTestingButton.render(lang)}"
                }
                ExtLink { href: GOOGLE_PLAY_URL, button: true, primary: true,
                    Icon { icon: FaGooglePlay }
                    "{MsgGooglePlayButton.render(lang)}"
                }
                ExtLink { href: APK_URL, button: true, primary: true,
                    Icon { icon: FaAndroid }
                    "{MsgDownloadApkButton.render(lang)}"
                }
                ExtLink { href: SOURCE_CODE_URL, button: true, primary: true,
                    Icon { icon: FaGithub }
                    "{MsgSourceCodeButton.render(lang)}"
                }
                ExtLink { href: FUNCTORA_URL, button: true, primary: true,
                    Icon { icon: FaUser }
                    "{MsgAuthorButton.render(lang)}"
                }
                NavLink {
                    nav,
                    href: Screen::Donate.to_route(None).to_string(),
                    button: true,
                    primary: true,
                    Icon { icon: FaHeart }
                    "{MsgDonateButton.render(lang)}"
                }
            }
        }
    }
}
