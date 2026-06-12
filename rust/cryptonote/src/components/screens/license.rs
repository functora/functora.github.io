use crate::messages::*;
use crate::*;

#[component]
pub fn License() -> Element {
    let lang = use_lang();

    rsx! {
        Breadcrumb { title: Msg::TermsOfServiceTitle }
        section {
            Pre { "{Msg::LicenseText.render(lang)}" }
            Dock {}
        }
    }
}
