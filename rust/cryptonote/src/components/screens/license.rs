use crate::messages::MsgTermsOfServiceTitle;
use crate::*;

#[component]
pub fn License() -> Element {
    let t = use_translations();

    rsx! {
        Breadcrumb { title: MsgTermsOfServiceTitle }
        section {
            Pre { "{t.license_text}" }
            Dock {}
        }
    }
}
