#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Attach,
    Zip,
    Encrypt,
    Decrypt,
    Unzip,
    Download,
}

pub type Job = functora_dioxus::progress::Job<Stage>;

pub use functora_dioxus::progress::{clear_progress, report, report_progress, report_progress_named, yield_to_paint};
