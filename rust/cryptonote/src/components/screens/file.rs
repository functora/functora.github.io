use crate::messages::*;
use crate::*;
use functora_dioxus::files::Preview;

#[component]
pub fn File() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let message = use_message();
    let preview = use_memo(move || {
        tst.attachment()()
            .and_then(|i| tst.attachments()().get(i).cloned())
            .map_or(functora_dioxus::files::Preview::Missing, |att| {
                functora_dioxus::files::preview(&att.name, &att.data)
            })
    });
    let atts = tst.attachments()();
    let idx = tst.attachment()();
    let att = idx.and_then(|i| atts.get(i));
    let name = att.map(|a| a.name.clone()).unwrap_or_default();
    let size = att.map(|a| format_size(a.data.len() as u64)).unwrap_or_default();

    let download = move |_| {
        if let Some(download_att) = tst.attachment()().and_then(|i| tst.attachments()().get(i).cloned()) {
            download_attachment(download_att, tst.progress(), message);
        }
    };

    rsx! {
        Breadcrumb { title: Msg::File }
        section {
            h3 { "{name}" }
            small { "{size}" }
            match &preview() {
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
