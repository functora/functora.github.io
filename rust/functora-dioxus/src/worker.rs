use crate::progress::Job;
use dioxus::prelude::{Writable, WritableExt};
use std::future::Future;
#[cfg(not(target_arch = "wasm32"))]
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Arc, Mutex};
#[cfg(not(target_arch = "wasm32"))]
use std::task::{Context, Poll, Wake, Waker};

#[cfg(not(target_arch = "wasm32"))]
pub type Reporter<S> = Box<dyn FnMut(Job<S>) + Send>;

#[cfg(target_arch = "wasm32")]
pub type Reporter<S> = Box<dyn FnMut(Job<S>)>;

#[cfg(not(target_arch = "wasm32"))]
enum WorkerMsg<S, O, E> {
    Job(Job<S>),
    Done(Result<O, E>),
}

#[cfg(not(target_arch = "wasm32"))]
struct ThreadWake(std::thread::Thread);

#[cfg(not(target_arch = "wasm32"))]
impl Wake for ThreadWake {
    fn wake(self: Arc<Self>) {
        self.0.unpark();
    }

    fn wake_by_ref(self: &Arc<Self>) {
        self.0.unpark();
    }
}

#[cfg(not(target_arch = "wasm32"))]
type WakeSlot = Arc<Mutex<Option<Waker>>>;

#[cfg(not(target_arch = "wasm32"))]
pub async fn run<J, O, E, S, F, M, P>(arg: J, progress: P, make: M) -> Result<O, E>
where
    J: Send + 'static,
    O: Send + 'static,
    E: From<String> + Send + 'static,
    S: Send + 'static,
    F: Future<Output = Result<O, E>> + Send + 'static,
    M: FnOnce(J, Reporter<S>) -> F + Send + 'static,
    P: Writable<Target = Option<Job<S>>>,
{
    let (tx, rx) = mpsc::channel();
    let wake_slot: WakeSlot = Arc::new(Mutex::new(None));
    _ = std::thread::spawn({
        let wake_slot_thread = wake_slot.clone();
        let send = tx.clone();
        let report_wake = wake_slot.clone();
        let report: Reporter<S> = Box::new(move |job| {
            _ = send.send(WorkerMsg::Job(job));
            wake_once(&report_wake);
        });
        move || drive(make(arg, report), &tx, &wake_slot_thread)
    });
    pump(rx, progress, wake_slot).await
}

#[cfg(not(target_arch = "wasm32"))]
fn wake_once(slot: &WakeSlot) {
    if let Ok(mut guard) = slot.lock()
        && let Some(waker) = guard.take()
    {
        waker.wake();
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn drive<S, O, E, F>(core: F, tx: &Sender<WorkerMsg<S, O, E>>, wake_slot: &WakeSlot)
where
    O: Send + 'static,
    E: Send + 'static,
    F: Future<Output = Result<O, E>> + Send + 'static,
{
    let mut pinned = Box::pin(core);
    let waker = Waker::from(Arc::new(ThreadWake(std::thread::current())));
    let mut cx = Context::from_waker(&waker);
    loop {
        match pinned.as_mut().poll(&mut cx) {
            Poll::Ready(result) => {
                _ = tx.send(WorkerMsg::Done(result));
                wake_once(wake_slot);
                return;
            }
            Poll::Pending => std::thread::park(),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn pump<S: 'static, O, E, P>(
    rx: Receiver<WorkerMsg<S, O, E>>,
    mut progress: P,
    wake_slot: WakeSlot,
) -> impl Future<Output = Result<O, E>>
where
    E: From<String>,
    P: Writable<Target = Option<Job<S>>>,
{
    std::future::poll_fn(move |cx| {
        if let Ok(mut slot) = wake_slot.lock() {
            *slot = Some(cx.waker().clone());
        }
        loop {
            match rx.try_recv() {
                Ok(WorkerMsg::Job(job)) => {
                    progress.set(Some(job));
                }
                Ok(WorkerMsg::Done(result)) => return Poll::Ready(result),
                Err(TryRecvError::Empty) => return Poll::Pending,
                Err(TryRecvError::Disconnected) => {
                    return Poll::Ready(Err("Background task stopped unexpectedly".to_string().into()));
                }
            }
        }
    })
}

#[cfg(target_arch = "wasm32")]
pub async fn run<J, O, E, S, F, M, P>(arg: J, mut progress: P, make: M) -> Result<O, E>
where
    J: 'static,
    O: 'static,
    E: 'static,
    S: 'static,
    F: Future<Output = Result<O, E>> + 'static,
    M: FnOnce(J, Reporter<S>) -> F + 'static,
    P: Writable<Target = Option<Job<S>>> + 'static,
{
    let reporter: Reporter<S> = Box::new(move |job| progress.set(Some(job)));
    make(arg, reporter).await
}
