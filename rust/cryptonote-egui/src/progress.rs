pub use functora_core::progress::{Job, Stage, YieldOnce, yield_to_paint};
pub use functora_egui::progress::{
    JobGuard, claim_job, clear_progress, report, report_progress, report_progress_named,
};

pub type JobProgress = Job<Stage>;
