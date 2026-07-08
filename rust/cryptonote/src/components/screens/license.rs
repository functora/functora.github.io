use crate::messages::*;
use crate::*;

#[component]
pub fn License() -> Element {
    rsx! {
        StaticPage { title: Msg::TermsOfServiceTitle, content: Msg::LicenseText }
    }
}
