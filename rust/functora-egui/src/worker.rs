use crate::error::Error;
use crate::progress::Job;
use functora_core::worker::run as core_run;
use std::future::Future;

pub use functora_core::worker::Reporter;

#[cfg(not(target_arch = "wasm32"))]
pub async fn run<J, O, E, S, F, M>(
    arg: J,
    progress: impl FnMut(Option<Job<S>>) + 'static,
    make: M,
) -> Result<O, E>
where
    J: Send + 'static,
    O: Send + 'static,
    S: Send + 'static,
    E: From<Error> + Send + 'static,
    F: Future<Output = Result<O, E>> + Send + 'static,
    M: FnOnce(J, Reporter<S>) -> F + Send + 'static,
{
    core_run(arg, progress, make).await
}

#[cfg(target_arch = "wasm32")]
pub async fn run<J, O, E, S, F, M>(
    arg: J,
    progress: impl FnMut(Option<Job<S>>) + 'static,
    make: M,
) -> Result<O, E>
where
    J: 'static,
    O: 'static,
    S: 'static,
    E: From<Error> + 'static,
    F: Future<Output = Result<O, E>> + 'static,
    M: FnOnce(J, Reporter<S>) -> F + 'static,
{
    core_run(arg, progress, make).await
}
