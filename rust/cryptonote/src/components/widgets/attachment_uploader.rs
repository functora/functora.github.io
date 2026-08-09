#![allow(clippy::shadow_reuse)]
use crate::messages::Msg;
use crate::*;
use functora_dioxus::files::preview;

#[component]
pub fn AttachmentUploader(tst: Store<TemporaryState>, lang: Language) -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let previews = use_memo(move || {
        tst.attachments()()
            .iter()
            .map(|a| preview(&a.name, &a.data))
            .collect::<Vec<_>>()
    });
    rsx! {
        functora_dioxus::widgets::AttachmentUploader {
            attachments: tst.attachments()(),
            previews: previews(),
            remove_file: Msg::RemoveFile.render(lang),
            on_open: move |i| {
                tst.attachment().set(Some(i));
                nav.write().push(Screen::File.to_route(None));
            },
            on_remove: move |i| remove_attachment(tst, i),
        }
    }
}
