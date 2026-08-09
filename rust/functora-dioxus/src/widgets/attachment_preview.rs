#![allow(clippy::shadow_reuse)]
use crate::files::Preview;
use dioxus::prelude::*;

#[component]
pub fn AttachmentPreview(name: String, preview: Preview) -> Element {
    match preview {
        Preview::Image(url) => rsx! {
            img { src: "{url}", alt: "{name}", style: "max-height: 6rem; width: auto;" }
        },
        Preview::Video(url) => rsx! {
            video {
                preload: "metadata",
                style: "max-height: 6rem; width: auto;",
                src: "{url}",
            }
        },
        _ => rsx! {},
    }
}
