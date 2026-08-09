use crate::messages::*;
use crate::*;

#[component]
pub fn View() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let mut message = use_message();

    let _ = use_effect(move || {
        if tst.note()().is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl.into())));
        }
    });

    rsx! {
        Breadcrumb { title: Msg::Note }
        NoteDisplay {}
    }
}
