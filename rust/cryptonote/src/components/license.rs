use crate::*;

#[component]
pub fn License() -> Element {
    let language = use_context::<Signal<Language>>();
    let translations = get_translations(language());
    let message = use_signal(|| Option::<UiMessage>::None);

    rsx! {
        Breadcrumb { title: translations.terms_of_service_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{translations.license_text}" }

            ActionRow { message }
        }
    }
}
