pub use functora_core::progress::{Job, Stage, YieldOnce, yield_to_paint};

pub fn report<S>(progress: &mut Option<Job<S>>, stage: S, done: u64, total: u64)
where
    S: Clone,
{
    *progress = Some(Job {
        stage,
        done,
        total,
        name: None,
    });
}

pub async fn report_progress<S>(progress: &mut Option<Job<S>>, stage: S, done: u64, total: u64)
where
    S: Clone,
{
    *progress = Some(Job {
        stage,
        done,
        total,
        name: None,
    });
    yield_to_paint().await;
    YieldOnce::new(false).await;
}

pub async fn report_progress_named<S>(
    progress: &mut Option<Job<S>>,
    stage: S,
    done: u64,
    total: u64,
    name: &str,
) where
    S: Clone,
{
    *progress = Some(Job {
        stage,
        done,
        total,
        name: Some(name.to_string()),
    });
    yield_to_paint().await;
    YieldOnce::new(false).await;
}

pub fn clear_progress<S>(progress: &mut Option<Job<S>>) {
    *progress = None;
}

pub struct JobGuard<'a, S> {
    slot: &'a mut Option<Job<S>>,
}

impl<S> Drop for JobGuard<'_, S> {
    fn drop(&mut self) {
        *self.slot = None;
    }
}

#[must_use]
pub fn claim_job<S>(slot: &mut Option<Job<S>>, stage: S) -> Option<JobGuard<'_, S>>
where
    S: Clone,
{
    if slot.is_some() {
        return None;
    }
    *slot = Some(Job {
        stage,
        done: 0,
        total: 1,
        name: None,
    });
    Some(JobGuard { slot })
}

pub fn with_callback<S, F>(mut callback: F) -> impl FnMut(Option<Job<S>>)
where
    F: FnMut(Option<Job<S>>),
    S: Clone,
{
    move |job| callback(job)
}
