use crate::messages::*;
use crate::*;

#[component]
pub fn Privacy() -> Element {
    rsx! {
        StaticPage { title: Msg::PrivacyPolicyTitle, content: Msg::PrivacyText }
    }
}
