use crate::*;

#[component]
pub fn About() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);
    //
    // TODO : separate module with static urls
    //
    let group_url = "https://groups.google.com/g/functora";
    let gplay_url = "https://play.google.com/store/apps/details?id=com.functora.cryptonote";
    let apk_url = format!(
        "https://github.com/functora/functora.github.io/releases/tag/cryptonote-v{}",
        env!("CARGO_PKG_VERSION")
    );
    let author_url = "https://groups.google.com/g/functora";
    let source_url = "https://github.com/functora/functora.github.io/tree/master/rust/cryptonote";

    rsx! {
        Breadcrumb { title: t.about_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{t.about_text}" }
            br {}
            br {}
            p {
                "{t.about_android_beta_1} "
                ExtLink { href: group_url, "{t.about_android_beta_link_1}" }
                " {t.about_android_beta_2} "
                ExtLink { href: gplay_url, "{t.about_android_beta_link_2}" }
                "{t.about_android_beta_3} "
                ExtLink { href: apk_url.clone(), "{t.about_android_beta_link_3}" }
                " {t.about_android_beta_4}"
            }
            Dock {
                ExtLink { href: group_url, button: true, primary: true,
                    Icon { icon: FaGoogle }
                    "{t.join_testing_button}"
                }
                ExtLink { href: gplay_url, button: true, primary: true,
                    Icon { icon: FaGooglePlay }
                    "{t.google_play_button}"
                }
                ExtLink { href: apk_url, button: true, primary: true,
                    Icon { icon: FaAndroid }
                    "{t.download_apk_button}"
                }
                ExtLink { href: source_url, button: true, primary: true,
                    Icon { icon: FaGithub }
                    "{t.source_code_button}"
                }
                ExtLink { href: author_url, button: true, primary: true,
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
