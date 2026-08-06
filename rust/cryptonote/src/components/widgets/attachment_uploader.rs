use crate::messages::Msg;
use crate::*;

#[component]
pub fn AttachmentUploader(tst: Store<TemporaryState>, lang: Language) -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    rsx! {
        functora_dioxus::widgets::AttachmentUploader {
            attachments: tst.attachments()(),
            file_name: Msg::FileName.render(lang),
            file_size: Msg::FileSize.render(lang),
            remove_file: Msg::RemoveFile.render(lang),
            on_open: move |i| {
                tst.attachment().set(Some(i));
                nav.write().push(Screen::File.to_route(None));
            },
            on_remove: move |i| remove_attachment(tst, i),
        }
    }
}
