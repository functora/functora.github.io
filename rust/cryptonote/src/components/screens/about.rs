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
                ExtLink { href: "https://groups.google.com/g/functora", "{t.about_android_beta_link_1}" }
                " {t.about_android_beta_2} "
                ExtLink { href: "https://play.google.com/store/apps/details?id=com.functora.cryptonote",
                    "{t.about_android_beta_link_2}"
                }
                "{t.about_android_beta_3} "
                ExtLink {
                    href: format!(
                        "https://github.com/functora/functora.github.io/releases/tag/cryptonote-v{}",
                        env!("CARGO_PKG_VERSION"),
                    ),
                    "{t.about_android_beta_link_3}"
                }
                " {t.about_android_beta_4}"
            }
            Dock {}
        }
    }
}
