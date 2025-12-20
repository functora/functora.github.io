use crate::*;

#[component]
pub fn Privacy() -> Element {
    let app_settings = use_context::<Signal<AppCfg>>();
    let translations =
        get_translations(app_settings.read().language);
    let message = use_signal(|| Option::<UiMessage>::None);

    rsx! {
        Breadcrumb { title: translations.privacy_policy_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{translations.privacy_text}" }

            Dock { message }
        }
    }
}
