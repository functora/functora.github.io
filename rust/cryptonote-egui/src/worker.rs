use crate::progress::Stage;

pub type Reporter = functora_egui::worker::Reporter<Stage>;

pub use functora_egui::worker::run;
