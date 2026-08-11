#![allow(clippy::shadow_reuse)]
use crate::files::Preview;
use dioxus::prelude::*;

#[component]
pub fn AttachmentPreview(name: String, preview: Preview, onclick: Option<EventHandler<MouseEvent>>) -> Element {
    let handle = move |evt: MouseEvent| {
        if let Some(f) = &onclick {
            f.call(evt);
        }
    };
    match preview {
        Preview::Image(url) => rsx! {
            img {
                src: "{url}",
                alt: "{name}",
                style: "max-height: 6rem; width: auto; cursor: pointer;",
                onclick: handle,
            }
        },
        Preview::Video(url) => rsx! {
            video {
                preload: "metadata",
                style: "max-height: 6rem; width: auto; cursor: pointer;",
                src: "{url}",
                onclick: handle,
            }
        },
        _ => rsx! {},
    }
}
