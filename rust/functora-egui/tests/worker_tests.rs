//! `worker::run` executes the future, reports progress through the
//! callback, and propagates errors.
#![cfg(feature = "runtime")]

use functora_egui::error::Error;
use functora_egui::progress::Job;
use std::sync::{Arc, Mutex};

fn run<F>(future: F) -> F::Output
where
    F: std::future::Future,
{
    pollster::block_on(future)
}

#[test]
fn worker_run_returns_value_and_reports_progress() {
    let seen: Arc<Mutex<Vec<Option<Job<String>>>>> = Arc::new(Mutex::new(Vec::new()));
    let seen_write = Arc::clone(&seen);
    let out: Result<u32, Error> = run(functora_egui::worker::run(
        21u32,
        move |job| {
            seen_write
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(job);
        },
        |arg, mut report| async move {
            report(Job {
                stage: "half".to_string(),
                done: 1,
                total: 2,
                name: None,
            });
            Ok(arg * 2)
        },
    ));
    assert_eq!(out.unwrap_or(0), 42);
    let reported = seen
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        reported
            .iter()
            .flatten()
            .any(|job| job.done == 1 && job.total == 2),
        "worker must forward reported jobs, got {reported:?}"
    );
}

#[test]
fn worker_run_propagates_errors() {
    let out: Result<u32, Error> = run(functora_egui::worker::run(
        (),
        |_: Option<Job<String>>| {},
        |(), _| async move { Err::<u32, Error>(Error::Cancelled) },
    ));
    assert!(matches!(out, Err(Error::Cancelled)));
}
