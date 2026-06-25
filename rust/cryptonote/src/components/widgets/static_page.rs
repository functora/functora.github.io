use crate::messages::Msg;
use crate::*;

#[component]
pub fn StaticPage(title: Msg, content: Msg) -> Element {
    let lang = use_lang();
    rsx! {
        Breadcrumb { title }
        section {
            Pre { "{content.render(lang)}" }
            Dock {}
        }
    }
}
