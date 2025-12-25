use crate::*;

#[component]
pub fn About() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);
    rsx! {
        Breadcrumb { title: t.about_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{t.about_text}" }
            br {}
            br {}
            p {
                "{t.about_android_beta_1} "
                ExtLink { href: BETA_TEST_URL, "{t.about_android_beta_link_1}" }
                " {t.about_android_beta_2} "
                ExtLink { href: GOOGLE_PLAY_URL, "{t.about_android_beta_link_2}" }
                "{t.about_android_beta_3} "
                ExtLink { href: APK_URL, "{t.about_android_beta_link_3}" }
                " {t.about_android_beta_4}"
            }
            Dock {
                ExtLink { href: BETA_TEST_URL, button: true, primary: true,
                    Icon { icon: FaGoogle }
                    "{t.join_testing_button}"
                }
                ExtLink { href: GOOGLE_PLAY_URL, button: true, primary: true,
                    Icon { icon: FaGooglePlay }
                    "{t.google_play_button}"
                }
                ExtLink { href: APK_URL, button: true, primary: true,
                    Icon { icon: FaAndroid }
                    "{t.download_apk_button}"
                }
                ExtLink { href: SOURCE_CODE_URL, button: true, primary: true,
                    Icon { icon: FaGithub }
                    "{t.source_code_button}"
                }
                ExtLink { href: FUNCTORA_URL, button: true, primary: true,
                    Icon { icon: FaUser }
                    "{t.author_button}"
                }
                NavLink {
                    route: Screen::Donate.to_route(None),
                    button: true,
                    primary: true,
                    Icon { icon: FaHeart }
                    "{t.donate_button}"
                }
            }
        }
    }
}
