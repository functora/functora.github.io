use dioxus::prelude::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Stage {
    #[default]
    Attach,
    Zip,
    Encrypt,
    Decrypt,
    Unzip,
    Download,
    Preview,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Job<S> {
    pub stage: S,
    pub done: u64,
    pub total: u64,
    pub name: Option<String>,
}

impl<S> Job<S> {
    pub fn percent(&self) -> u8 {
        (self.done * 100 / self.total.max(1)).min(100) as u8
    }
}

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
    YieldOnce(false).await;
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
    YieldOnce(false).await;
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

pub async fn yield_to_paint() {
    #[cfg(target_arch = "wasm32")]
    {
        let mut eval = dioxus::document::eval("await new Promise(r => setTimeout(r, 1)); dioxus.send('tick')");
        _ = eval.recv::<String>().await;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        YieldOnce(false).await;
    }
}

pub struct YieldOnce(bool);

impl Future for YieldOnce {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}
