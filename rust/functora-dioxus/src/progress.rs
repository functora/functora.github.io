pub use functora_core::progress::{Job, Stage, YieldOnce, yield_to_paint};

use dioxus::prelude::*;

pub fn report<P, S>(mut progress: P, stage: S, done: u64, total: u64)
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    progress.set(Some(Job {
        stage,
        done,
        total,
        name: None,
    }));
}

pub async fn report_progress<P, S>(mut progress: P, stage: S, done: u64, total: u64)
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    progress.set(Some(Job {
        stage,
        done,
        total,
        name: None,
    }));
    yield_to_paint().await;
    YieldOnce::new(false).await;
}

pub async fn report_progress_named<P, S>(mut progress: P, stage: S, done: u64, total: u64, name: &str)
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    progress.set(Some(Job {
        stage,
        done,
        total,
        name: Some(name.to_string()),
    }));
    yield_to_paint().await;
    YieldOnce::new(false).await;
}

pub fn clear_progress<P, S>(mut progress: P)
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    progress.set(None);
}

/// RAII ownership of the single-job progress slot. While a guard is held no other
/// job can start: `claim_job` rejects every concurrent attempt, and the slot is
/// released when the guard drops, so a task dropped by unmounting the screen never
/// leaves the app stuck with a permanently claimed job.
pub struct JobGuard<P, S>(P)
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static;

impl<P, S> Drop for JobGuard<P, S>
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    fn drop(&mut self) {
        self.0.set(None);
    }
}

#[must_use]
pub fn claim_job<P, S>(mut progress: P, stage: S) -> Option<JobGuard<P, S>>
where
    P: Writable<Target = Option<Job<S>>>,
    S: 'static,
{
    if progress.peek().is_some() {
        return None;
    }
    progress.set(Some(Job {
        stage,
        done: 0,
        total: 1,
        name: None,
    }));
    Some(JobGuard(progress))
}
