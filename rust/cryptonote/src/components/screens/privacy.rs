use crate::*;

#[component]
pub fn Privacy() -> Element {
    let t = use_translations();

    rsx! {
        Breadcrumb { title: t.privacy_policy_title.to_string() }
        section {
            Pre { "{t.privacy_text}" }
            Dock {}
        }
    }
}
