use crate::progress::Stage;

pub type Reporter = functora_dioxus::worker::Reporter<Stage>;

pub use functora_dioxus::worker::run;
