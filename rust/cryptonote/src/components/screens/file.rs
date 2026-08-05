use crate::messages::*;
use crate::*;
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;

#[derive(Debug, Clone, PartialEq)]
enum Preview {
    Image(String),
    Video(String),
    Audio(String),
    Pdf(String),
    Markdown(String),
    Text(String),
    Download,
    Missing,
}

fn mime_for(name: &str) -> Option<&'static str> {
    let ext = name
        .rsplit_once('.')
        .map(|(_, e)| e.to_ascii_lowercase())
        .unwrap_or_default();
    match ext.as_str() {
        "jpg" | "jpeg" => Some("image/jpeg"),
        "png" => Some("image/png"),
        "gif" => Some("image/gif"),
        "webp" => Some("image/webp"),
        "bmp" => Some("image/bmp"),
        "svg" => Some("image/svg+xml"),
        "avif" => Some("image/avif"),
        "ico" => Some("image/x-icon"),
        "mp4" => Some("video/mp4"),
        "webm" => Some("video/webm"),
        "mov" => Some("video/quicktime"),
        "ogv" => Some("video/ogg"),
        "m4v" => Some("video/x-m4v"),
        "mp3" => Some("audio/mpeg"),
        "wav" => Some("audio/wav"),
        "ogg" | "oga" => Some("audio/ogg"),
        "m4a" => Some("audio/mp4"),
        "flac" => Some("audio/flac"),
        "aac" => Some("audio/aac"),
        "opus" => Some("audio/opus"),
        "pdf" => Some("application/pdf"),
        "txt" | "log" => Some("text/plain"),
        "md" | "markdown" => Some("text/markdown"),
        "html" | "htm" => Some("text/html"),
        "css" => Some("text/css"),
        "csv" => Some("text/csv"),
        "json" => Some("application/json"),
        "xml" => Some("application/xml"),
        "toml" => Some("application/toml"),
        "yaml" | "yml" => Some("application/yaml"),
        _ => None,
    }
}

fn is_text(mime: &str) -> bool {
    mime.starts_with("text/")
        || matches!(
            mime,
            "application/json" | "application/xml" | "application/toml" | "application/yaml"
        )
}

fn build_preview(tst: Store<TemporaryState>) -> Preview {
    let Some(att) = tst.attachment()().and_then(|i| tst.attachments()().get(i).cloned()) else {
        return Preview::Missing;
    };
    let Some(mime) = mime_for(&att.name) else {
        return Preview::Download;
    };
    let url = format!("data:{mime};base64,{}", BASE64.encode(&att.data));
    if mime.starts_with("image/") {
        return Preview::Image(url);
    }
    if mime.starts_with("video/") {
        return Preview::Video(url);
    }
    if mime.starts_with("audio/") {
        return Preview::Audio(url);
    }
    if mime == "application/pdf" {
        return Preview::Pdf(url);
    }
    if is_text(mime) {
        return match String::from_utf8(att.data) {
            Ok(text) if mime == "text/markdown" => Preview::Markdown(text),
            Ok(text) => Preview::Text(text),
            Err(_) => Preview::Download,
        };
    }
    Preview::Download
}

#[component]
pub fn File() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let message = use_message();
    let preview = use_memo(move || build_preview(tst));
    let atts = tst.attachments()();
    let idx = tst.attachment()();
    let att = idx.and_then(|i| atts.get(i));
    let name = att.map(|a| a.name.clone()).unwrap_or_default();
    let size = att.map(|a| format_size(a.data.len() as u64)).unwrap_or_default();

    let download = move |_| {
        if let Some(att) = tst.attachment()().and_then(|i| tst.attachments()().get(i).cloned()) {
            download_attachment(att, tst.progress(), message);
        }
    };

    rsx! {
        Breadcrumb { title: Msg::File }
        section {
            card {
                h3 { "{name}" }
                small { "{size}" }
                match &preview() {
                    Preview::Image(url) => rsx! {
                        img { src: "{url}", alt: "{name}" }
                    },
                    Preview::Video(url) => rsx! {
                        video { controls: true, src: "{url}" }
                    },
                    Preview::Audio(url) => rsx! {
                        audio { controls: true, src: "{url}" }
                    },
                    Preview::Pdf(url) => rsx! {
                        iframe { src: "{url}", style: "width: 100%; height: 80vh; border: none;" }
                    },
                    Preview::Markdown(text) => rsx! {
                        div {
                            overflow_wrap: "anywhere",
                            word_break: "break-word",
                            dangerous_inner_html: render_markdown(text),
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
                }
            }

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
