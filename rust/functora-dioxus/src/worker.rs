pub use functora_core::worker::Reporter;

use crate::Error;
use crate::progress::Job;
use dioxus::prelude::{Writable, WritableExt};
use functora_core::worker::run as core_run;
use std::future::Future;

#[cfg(not(target_arch = "wasm32"))]
pub async fn run<J, O, E, S, F, M, P>(arg: J, mut progress: P, make: M) -> Result<O, E>
where
    J: Send + 'static,
    O: Send + 'static,
    S: Send + 'static,
    E: From<Error> + Send + 'static,
    F: Future<Output = Result<O, E>> + Send + 'static,
    M: FnOnce(J, Reporter<S>) -> F + Send + 'static,
    P: Writable<Target = Option<Job<S>>> + 'static,
{
    core_run(arg, move |job| progress.set(job), make).await
}

#[cfg(target_arch = "wasm32")]
pub async fn run<J, O, E, S, F, M, P>(arg: J, mut progress: P, make: M) -> Result<O, E>
where
    J: 'static,
    O: 'static,
    S: 'static,
    E: From<Error> + 'static,
    F: Future<Output = Result<O, E>> + 'static,
    M: FnOnce(J, Reporter<S>) -> F + 'static,
    P: Writable<Target = Option<Job<S>>> + 'static,
{
    core_run(arg, move |job| progress.set(job), make).await
}
