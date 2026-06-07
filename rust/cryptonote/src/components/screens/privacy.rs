use crate::messages::*;
use crate::*;

#[component]
pub fn Privacy() -> Element {
    let lang = use_lang();

    rsx! {
        Breadcrumb { title: MsgPrivacyPolicyTitle }
        section {
            Pre { "{MsgPrivacyText.render(lang)}" }
            Dock {}
        }
    }
}
