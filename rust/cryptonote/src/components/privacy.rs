use crate::*;

#[component]
pub fn Privacy() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);
    let message = use_signal(|| Option::<UiMessage>::None);

    rsx! {
        Breadcrumb { title: t.privacy_policy_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{t.privacy_text}" }
            Dock { message }
        }
    }
}
