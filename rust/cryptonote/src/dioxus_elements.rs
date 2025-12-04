pub use crate::dioxus_elements;
pub use elements::*;

pub mod elements {
    #![allow(non_camel_case_types)]
    pub struct card;
    pub use dioxus::prelude::dioxus_elements::*;
    pub use dioxus::prelude::{
        Element, GlobalSignal, IntoDynNode, Readable,
        Signal, Writable, component, dioxus_core,
        navigator, rsx, use_context, use_effect,
        use_signal,
    };

    impl card {
        pub const TAG_NAME: &'static str = "card";
        pub const NAME_SPACE: Option<&'static str> = None;
    }
}
