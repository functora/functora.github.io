#![allow(clippy::shadow_reuse)]
use crate::dioxus_elements;
use dioxus::prelude::*;

#[component]
pub fn DefaultHome(brand: String, tagline: String) -> Element {
    rsx! {
        card { "fs": "l",
            section {
                p { "txt": "c", "{brand}" }
                p { "txt": "c", "{tagline}" }
            }
        }
    }
}
