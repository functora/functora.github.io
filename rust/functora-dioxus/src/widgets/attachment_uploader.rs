#![allow(clippy::shadow_reuse)]
use crate::files::{Attachment, format_size};
use dioxus::prelude::*;
use dioxus_free_icons::Icon;
use dioxus_free_icons::icons::fa_solid_icons::FaXmark;

#[component]
pub fn AttachmentUploader(
    attachments: Vec<Attachment>,
    file_name: String,
    file_size: String,
    remove_file: String,
    on_open: EventHandler<usize>,
    on_remove: EventHandler<usize>,
) -> Element {
    if attachments.is_empty() {
        return rsx! {};
    }
    rsx! {
        table {
            thead {
                tr {
                    th { "{file_name}" }
                    th { colspan: 2, "{file_size}" }
                }
            }
            tbody {
                for (i, att) in attachments.iter().enumerate() {
                    tr { key: "{i}",
                        td {
                            a { onclick: move |_| on_open.call(i), "{att.name}" }
                        }
                        td { "{format_size(att.data.len() as u64)}" }
                        td { "txt": "r",
                            button {
                                onclick: move |_| on_remove.call(i),
                                aria_label: "{remove_file}",
                                Icon { icon: FaXmark }
                            }
                        }
                    }
                }
            }
        }
    }
}
