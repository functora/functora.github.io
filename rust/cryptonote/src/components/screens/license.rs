use crate::messages::*;
use crate::*;

#[component]
pub fn License() -> Element {
    let lang = use_lang();

    rsx! {
        Breadcrumb { title: MsgTermsOfServiceTitle }
        section {
            Pre { "{MsgLicenseText.render(lang)}" }
            Dock {}
        }
    }
}
