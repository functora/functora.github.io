use crate::*;

#[component]
pub fn Privacy() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);

    rsx! {
        Breadcrumb { title: t.privacy_policy_title.to_string() }
        section {
            Pre { "{t.privacy_text}" }
            Dock {}
        }
    }
}
