#![allow(clippy::unwrap_used, clippy::expect_used)]

use functora_core::Error;
use functora_core::worker::{Reporter, run};
use std::task::{Context, Poll, Waker};
use std::time::{Duration, Instant};

#[test]
fn dropping_run_with_pending_worker_returns_immediately() {
    let waker = Waker::noop();
    let mut cx = Context::from_waker(waker);
    let mut fut = Box::pin(run(
        (),
        |_| {},
        |(), _report: Reporter<()>| async { std::future::pending::<Result<(), Error>>().await },
    ));
    assert!(matches!(fut.as_mut().poll(&mut cx), Poll::Pending));
    let start = Instant::now();
    drop(fut);
    assert!(start.elapsed() < Duration::from_secs(5));
}

#[test]
fn run_reports_worker_result() {
    let waker = Waker::noop();
    let mut cx = Context::from_waker(waker);
    let mut fut = Box::pin(run(
        (),
        |_| {},
        |(), _report: Reporter<()>| async { Ok::<u8, Error>(7) },
    ));
    let result = loop {
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(result) => break result,
            Poll::Pending => std::thread::yield_now(),
        }
    };
    assert_eq!(result.unwrap(), 7);
}
