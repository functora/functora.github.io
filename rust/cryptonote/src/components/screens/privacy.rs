use crate::messages::*;
use crate::*;

#[component]
pub fn Privacy() -> Element {
    let lang = use_lang();

    rsx! {
        Breadcrumb { title: Msg::PrivacyPolicyTitle }
        section {
            Pre { "{Msg::PrivacyText.render(lang)}" }
            Dock {}
        }
    }
}
