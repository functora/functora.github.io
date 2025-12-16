pub use dioxus::prelude::*;
pub use serde::{Deserialize, Serialize};
pub mod dioxus_elements {
    pub use elements::*;
    pub mod elements {
        pub use dioxus::prelude::dioxus_elements::*;
        pub mod card {
            pub use dioxus::prelude::dioxus_elements::elements::template::*;
            pub const TAG_NAME: &str = "card";
            pub const NAME_SPACE: Option<&'static str> =
                None;
        }
    }
}
