use crate::messages::*;
use crate::*;
use functora_dioxus::files::{
    preview_blob, preview_blob_url, preview_cached, preview_initial, revoke_blob_url, Preview,
};
use functora_dioxus::widgets::AttachmentPreview;

#[component]
pub fn File() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let progress = tst.progress();
    let lang = use_lang();
    let message = use_message();
    let att_idx = tst.attachment()();
    let att = att_idx.and_then(|i| tst.attachments()().get(i).cloned());
    let name = att.as_ref().map(|a| a.name.clone()).unwrap_or_default();
    let size = att
        .as_ref()
        .map(|a| format_size(a.data.len() as u64))
        .unwrap_or_default();
    let initial = att.as_ref().and_then(|a| preview_initial(&a.name, &a.data));
    let loading_thumb = att.as_ref().and_then(|a| match preview_cached(&a.name, &a.data) {
        Preview::Video(url) => Some(url.clone()),
        _ => None,
    });
    let blob_preview: Signal<Option<Preview>> = use_signal(|| initial);
    let current_blob: Signal<Option<String>> = use_signal(|| None);
    let mut preview_gen: Signal<u64> = use_signal(|| 0);

    let _ = use_effect(move || {
        let idx = tst.attachment()();
        let eff_att = idx.and_then(|i| tst.attachments()().get(i).cloned());
        let prev = current_blob.peek().clone();
        let gen = *preview_gen.peek() + 1;
        preview_gen.set(gen);
        let mut blob_out = blob_preview;
        let mut current_out = current_blob;
        let gen_out = preview_gen;
        if let Some(sel) = eff_att {
            let is_stream = preview_initial(&sel.name, &sel.data).is_none();
            if is_stream {
                _ = spawn(async move {
                    if let Some(url) = prev {
                        if let Err(e) = revoke_blob_url(&url) {
                            tracing::warn!("Failed to revoke previous blob URL: {e}");
                        }
                    }
                    let preview = preview_blob(&sel.name, &sel.data, progress).await;
                    clear_progress(progress);
                    if gen_out() == gen {
                        blob_out.set(Some(preview.clone()));
                        current_out.set(preview_blob_url(&preview).map(str::to_string));
                    } else if let Some(url) = preview_blob_url(&preview) {
                        if let Err(e) = revoke_blob_url(url) {
                            tracing::warn!("Failed to revoke superseded blob URL: {e}");
                        }
                    }
                });
            } else if let Some(url) = prev {
                if let Err(e) = revoke_blob_url(&url) {
                    tracing::warn!("Failed to revoke previous blob URL: {e}");
                }
                current_out.set(None);
                clear_progress(progress);
                blob_out.set(preview_initial(&sel.name, &sel.data));
            } else {
                current_out.set(None);
                clear_progress(progress);
                blob_out.set(preview_initial(&sel.name, &sel.data));
            }
        } else if let Some(url) = prev {
            if let Err(e) = revoke_blob_url(&url) {
                tracing::warn!("Failed to revoke previous blob URL: {e}");
            }
            current_out.set(None);
            clear_progress(progress);
            blob_out.set(Some(Preview::Missing));
        } else {
            current_out.set(None);
            clear_progress(progress);
            blob_out.set(Some(Preview::Missing));
        }
    });

    use_drop(move || {
        if let Some(url) = current_blob.peek().clone() {
            if let Err(e) = revoke_blob_url(&url) {
                tracing::warn!("Failed to revoke blob URL on unmount: {e}");
            }
        }
    });

    let download = move |_| {
        if let Some(download_att) = tst.attachment()().and_then(|i| tst.attachments()().get(i).cloned()) {
            download_attachment(download_att, tst.progress(), message);
        }
    };

    let content = if att.is_none() {
        rsx! {
            Pre {
                code { "{Msg::FileNotFound.render(lang)}" }
            }
        }
    } else {
        match blob_preview() {
            None => match loading_thumb {
                Some(url) => rsx! {
                    AttachmentPreview {
                        name: name.clone(),
                        preview: Preview::Video(url),
                        onclick: None,
                        style: "display: block; width: 100%; height: auto;".to_string(),
                    }
                },
                None => rsx! {
                    Pre {
                        code { "{Msg::Base(BaseMsg::Stage(Stage::Preview)).render(lang)}" }
                    }
                },
            },
            Some(preview) => match preview {
                Preview::Image(url) => rsx! {
                    img { src: "{url}", alt: "{name}" }
                },
                Preview::Video(url) => rsx! {
                    video {
                        controls: true,
                        autoplay: true,
                        playsinline: true,
                        r#loop: true,
                        src: "{url}",
                    }
                },
                Preview::Audio(url) => rsx! {
                    audio {
                        controls: true,
                        autoplay: true,
                        r#loop: true,
                        src: "{url}",
                    }
                },
                Preview::Pdf(url) => rsx! {
                    iframe {
                        src: "{url}",
                        style: "width: 100%; height: 80vh; border: none;",
                    }
                },
                Preview::Markdown(text) => rsx! {
                    div {
                        overflow_wrap: "anywhere",
                        word_break: "break-word",
                        dangerous_inner_html: render_markdown(&text),
                    }
                },
                Preview::Text(text) => rsx! {
                    Pre {
                        code { "{text}" }
                    }
                },
                Preview::Download => rsx! {
                    Pre {
                        code { "{Msg::PreviewUnavailable.render(lang)}" }
                    }
                },
                Preview::Missing => rsx! {
                    Pre {
                        code { "{Msg::FileNotFound.render(lang)}" }
                    }
                },
            },
        }
    };

    rsx! {
        Breadcrumb { title: Msg::File }
        section {
            h3 { overflow_wrap: "anywhere", word_break: "break-word", "{name}" }
            small { "{size}" }
            {content}
            Dock { message,
                Button {
                    icon: Some(FaDownload),
                    primary: true,
                    onclick: download,
                    i18n: Some(Msg::Download),
                    lang,
                }
            }
        }
    }
}
