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

pub async fn yield_to_paint() {
    #[cfg(target_arch = "wasm32")]
    {
        gloo_timers::future::TimeoutFuture::new(1).await;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        YieldOnce(false).await;
    }
}

pub struct YieldOnce(bool);

impl YieldOnce {
    #[must_use]
    pub const fn new(ready: bool) -> Self {
        Self(ready)
    }
}

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
