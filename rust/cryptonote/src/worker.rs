use crate::error::AppError;
use crate::progress::Job;
use dioxus::prelude::{Writable, WritableExt};
use std::future::Future;
#[cfg(not(target_arch = "wasm32"))]
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Arc, Mutex};
#[cfg(not(target_arch = "wasm32"))]
use std::task::Poll;
#[cfg(not(target_arch = "wasm32"))]
use std::task::{Context, Wake, Waker};

#[cfg(not(target_arch = "wasm32"))]
pub type Reporter = Box<dyn FnMut(Job) + Send>;

#[cfg(target_arch = "wasm32")]
pub type Reporter = Box<dyn FnMut(Job)>;

#[cfg(not(target_arch = "wasm32"))]
enum WorkerMsg<O> {
    Job(Job),
    Done(Result<O, AppError>),
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
pub async fn run<J, O, F, S, P>(arg: J, progress: P, make: S) -> Result<O, AppError>
where
    J: Send + 'static,
    O: Send + 'static,
    F: Future<Output = Result<O, AppError>> + Send + 'static,
    S: FnOnce(J, Reporter) -> F + Send + 'static,
    P: Writable<Target = Option<Job>>,
{
    let (tx, rx) = mpsc::channel();
    let wake_slot: WakeSlot = Arc::new(Mutex::new(None));
    std::thread::spawn({
        let wake_slot = wake_slot.clone();
        let send = tx.clone();
        let report_wake = wake_slot.clone();
        let report: Reporter = Box::new(move |job| {
            let _ = send.send(WorkerMsg::Job(job));
            wake_once(&report_wake);
        });
        move || drive(make(arg, report), tx, wake_slot)
    });
    pump(rx, progress, wake_slot).await
}

#[cfg(not(target_arch = "wasm32"))]
fn wake_once(slot: &WakeSlot) {
    if let Ok(mut guard) = slot.lock() {
        if let Some(waker) = guard.take() {
            waker.wake();
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn drive<O, F>(core: F, tx: Sender<WorkerMsg<O>>, wake_slot: WakeSlot)
where
    O: Send + 'static,
    F: Future<Output = Result<O, AppError>> + Send + 'static,
{
    let mut core = Box::pin(core);
    let waker = Waker::from(Arc::new(ThreadWake(std::thread::current())));
    let mut cx = Context::from_waker(&waker);
    loop {
        match core.as_mut().poll(&mut cx) {
            Poll::Ready(result) => {
                let _ = tx.send(WorkerMsg::Done(result));
                wake_once(&wake_slot);
                return;
            }
            Poll::Pending => std::thread::park(),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn pump<O, P>(
    rx: Receiver<WorkerMsg<O>>,
    mut progress: P,
    wake_slot: WakeSlot,
) -> impl Future<Output = Result<O, AppError>>
where
    P: Writable<Target = Option<Job>>,
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
                    return Poll::Ready(Err(AppError::Archive("Background task stopped unexpectedly".into())))
                }
            }
        }
    })
}

#[cfg(target_arch = "wasm32")]
pub async fn run<J, O, F, S, P>(arg: J, mut progress: P, make: S) -> Result<O, AppError>
where
    O: 'static,
    F: Future<Output = Result<O, AppError>> + 'static,
    S: FnOnce(J, Reporter) -> F + 'static,
    P: Writable<Target = Option<Job>> + 'static,
{
    let reporter: Reporter = Box::new(move |job| progress.set(Some(job)));
    make(arg, reporter).await
}
