use crate::*;

#[component]
pub fn License() -> Element {
    let t = use_translations();

    rsx! {
        Breadcrumb { title: t.terms_of_service_title.to_string() }
        section {
            Pre { "{t.license_text}" }
            Dock {}
        }
    }
}
