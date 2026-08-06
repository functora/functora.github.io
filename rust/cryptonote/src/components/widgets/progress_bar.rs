use crate::messages::Msg;
use crate::*;

#[component]
pub fn ProgressBar() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    rsx! {
        JobProgressBar {
            job: tst.progress()(),
            stage_label: Callback::new(|s| Msg::Base(BaseMsg::Stage(s))),
            lang,
        }
    }
}
