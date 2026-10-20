use std::sync::mpsc::{Receiver, TryRecvError};

/// Outcome of a non-blocking channel poll.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PollOutcome<T, E> {
    Ready(Result<T, E>),
    Disconnected,
}

/// Polls an optional receiver slot without blocking.
///
/// Returns `None` when the slot is empty or the value has not arrived yet
/// (the receiver is restored into the slot so polling can resume). Returns
/// `Some` when a value is ready or the sender was dropped.
#[must_use]
pub fn poll_channel<T, E>(slot: &mut Option<Receiver<Result<T, E>>>) -> Option<PollOutcome<T, E>> {
    let rx = slot.take()?;
    match rx.try_recv() {
        Ok(value) => Some(PollOutcome::Ready(value)),
        Err(TryRecvError::Empty) => {
            *slot = Some(rx);
            None
        }
        Err(TryRecvError::Disconnected) => Some(PollOutcome::Disconnected),
    }
}
