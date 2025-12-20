use crate::*;

#[component]
pub fn License() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);
    let message = use_signal(|| Option::<UiMessage>::None);

    rsx! {
        Breadcrumb { title: t.terms_of_service_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{t.license_text}" }
            Dock { message }
        }
    }
}
