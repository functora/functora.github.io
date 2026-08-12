#![allow(clippy::shadow_reuse)]
use crate::files::{Preview, video_thumbnail};
use dioxus::prelude::*;

#[component]
pub fn AttachmentPreview(name: String, preview: Preview, onclick: Option<EventHandler<MouseEvent>>) -> Element {
    let handle_onclick = onclick;
    let handle = move |evt: MouseEvent| {
        if let Some(f) = &handle_onclick {
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
            VideoThumb { url, name, onclick }
        },
        _ => rsx! {},
    }
}

#[component]
fn VideoThumb(url: String, name: String, onclick: Option<EventHandler<MouseEvent>>) -> Element {
    let thumb = use_signal(|| Option::<String>::None);
    _ = use_effect(move || {
        let url_out = url.clone();
        let mut thumb_out = thumb;
        _ = spawn(async move {
            thumb_out.set(video_thumbnail(&url_out).await);
        });
    });
    match thumb() {
        Some(src) => rsx! {
            img {
                src: "{src}",
                alt: "{name}",
                style: "max-height: 6rem; width: auto; cursor: pointer;",
                onclick: move |evt: MouseEvent| {
                    if let Some(f) = &onclick {
                        f.call(evt);
                    }
                },
            }
        },
        None => rsx! {},
    }
}
