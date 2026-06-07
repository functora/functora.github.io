use crate::messages::MsgPrivacyPolicyTitle;
use crate::*;

#[component]
pub fn Privacy() -> Element {
    let t = use_translations();

    rsx! {
        Breadcrumb { title: MsgPrivacyPolicyTitle }
        section {
            Pre { "{t.privacy_text}" }
            Dock {}
        }
    }
}
