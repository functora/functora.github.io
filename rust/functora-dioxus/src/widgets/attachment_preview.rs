#![allow(clippy::shadow_reuse)]
use crate::files::{Preview, video_thumbnail};
use dioxus::prelude::*;

const DEFAULT_ATTACHMENT_STYLE: &str = "max-height: 6rem; width: auto; cursor: pointer;";

fn attachment_style(style: Option<&str>) -> &str {
    style.unwrap_or(DEFAULT_ATTACHMENT_STYLE)
}

#[component]
pub fn AttachmentPreview(
    name: String,
    preview: Preview,
    onclick: Option<EventHandler<MouseEvent>>,
    style: Option<String>,
) -> Element {
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
                style: attachment_style(style.as_deref()),
                onclick: handle,
            }
        },
        Preview::Video(url) => rsx! {
            VideoThumb { url, name, onclick, style }
        },
        _ => rsx! {},
    }
}

#[component]
fn VideoThumb(url: String, name: String, onclick: Option<EventHandler<MouseEvent>>, style: Option<String>) -> Element {
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
                style: attachment_style(style.as_deref()),
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
