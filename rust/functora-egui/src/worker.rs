use crate::error::Error;
use crate::progress::Job;
use functora_core::worker::run as core_run;
use std::future::Future;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

pub use functora_core::worker::Reporter;

#[cfg(not(target_arch = "wasm32"))]
pub async fn run<J, O, E, S, F, M>(
    arg: J,
    progress: impl FnMut(Option<Job<S>>) + 'static,
    cancel: Option<Arc<AtomicBool>>,
    make: M,
) -> Result<O, E>
where
    J: Send + 'static,
    O: Send + 'static,
    S: Send + 'static,
    E: From<Error> + Send + 'static,
    F: Future<Output = Result<O, E>> + Send + 'static,
    M: FnOnce(J, Reporter<S>, Option<Arc<AtomicBool>>) -> F + Send + 'static,
{
    core_run(arg, progress, cancel, make).await
}

#[cfg(target_arch = "wasm32")]
pub async fn run<J, O, E, S, F, M>(
    arg: J,
    progress: impl FnMut(Option<Job<S>>) + 'static,
    cancel: Option<Arc<AtomicBool>>,
    make: M,
) -> Result<O, E>
where
    J: 'static,
    O: 'static,
    S: 'static,
    E: From<Error> + 'static,
    F: Future<Output = Result<O, E>> + 'static,
    M: FnOnce(J, Reporter<S>, Option<Arc<AtomicBool>>) -> F + 'static,
{
    core_run(arg, progress, cancel, make).await
}

/// Spawns an async task and returns a receiver for the result.
/// Works on both wasm32 (using `wasm_bindgen_futures::spawn_local`)
/// and native (using `std::thread::spawn` with `pollster::block_on`).
#[cfg(target_arch = "wasm32")]
#[cfg(feature = "runtime")]
pub fn spawn_async<F, T>(future: F) -> std::sync::mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + 'static,
    T: 'static,
{
    let (tx, rx) = std::sync::mpsc::channel();
    wasm_bindgen_futures::spawn_local(async move {
        let res = future.await;
        drop(tx.send(res));
    });
    rx
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "runtime")]
pub fn spawn_async<F, T>(future: F) -> std::sync::mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    use std::sync::OnceLock;
    static POOL: OnceLock<threadpool::ThreadPool> = OnceLock::new();
    let pool = POOL.get_or_init(|| threadpool::ThreadPool::new(2));
    let (tx, rx) = std::sync::mpsc::channel();
    pool.execute(move || {
        let res = pollster::block_on(future);
        drop(tx.send(res));
    });
    rx
}
