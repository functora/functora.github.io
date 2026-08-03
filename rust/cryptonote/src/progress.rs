use dioxus::prelude::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Attach,
    Zip,
    Encrypt,
    Decrypt,
    Unzip,
    Download,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Job {
    pub stage: Stage,
    pub done: u64,
    pub total: u64,
    pub name: Option<String>,
}

impl Job {
    pub fn percent(&self) -> u8 {
        (self.done * 100 / self.total.max(1)).min(100) as u8
    }
}

pub fn report<P>(mut progress: P, stage: Stage, done: u64, total: u64)
where
    P: Writable<Target = Option<Job>>,
{
    progress.set(Some(Job {
        stage,
        done,
        total,
        name: None,
    }));
}

pub async fn report_progress<P>(mut progress: P, stage: Stage, done: u64, total: u64)
where
    P: Writable<Target = Option<Job>>,
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

pub async fn report_progress_named<P>(mut progress: P, stage: Stage, done: u64, total: u64, name: &str)
where
    P: Writable<Target = Option<Job>>,
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

pub fn clear_progress<P>(mut progress: P)
where
    P: Writable<Target = Option<Job>>,
{
    progress.set(None);
}

pub(crate) struct YieldOnce(pub bool);

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

#[cfg(target_arch = "wasm32")]
pub(crate) async fn yield_to_paint() {
    let mut eval = dioxus::document::eval("await new Promise(r => setTimeout(r, 1)); dioxus.send('tick')");
    let _ = eval.recv::<String>().await;
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) async fn yield_to_paint() {
    YieldOnce(false).await;
}
