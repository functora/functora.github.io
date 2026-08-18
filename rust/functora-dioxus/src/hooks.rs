use crate::Language;
use crate::i18n::I18N;
use crate::state::{PersistentState, PersistentStateStoreExt};
use crate::storage::PersistentSignal;
use dioxus::prelude::*;

#[must_use]
pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>().language()()
}

pub fn use_message_markdown<T: I18N + 'static>(msg: T) -> Memo<String> {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    use_memo(move || msg.render_markdown(pst.language()()))
}

#[must_use]
pub fn use_message<T: 'static>() -> Signal<Option<T>> {
    use_signal(|| None)
}

/// Reusable in-flight guard for one-shot actions (native share dialogs, printing,
/// etc.). `claim` rejects every concurrent attempt while a previous action is still
/// running, and the slot is released when the guard drops, so an action cancelled by
/// unmounting the screen can never leave the button permanently disabled.
#[must_use]
pub fn use_in_flight() -> InFlight {
    InFlight(use_signal(|| false))
}

#[derive(Clone, Copy)]
pub struct InFlight(Signal<bool>);

impl InFlight {
    #[must_use]
    pub fn claim(&mut self) -> Option<InFlightGuard> {
        if *self.0.peek() {
            return None;
        }
        self.0.set(true);
        Some(InFlightGuard(self.0))
    }

    /// Spawns `fut` only while the in-flight slot is free, holding the claim for the
    /// whole task; returns `None` when another action is still running.
    pub fn run(&mut self, fut: impl Future<Output = ()> + 'static) -> Option<dioxus::core::Task> {
        let guard = self.claim()?;
        Some(spawn_guarded(guard, fut))
    }
}

#[must_use]
pub struct InFlightGuard(Signal<bool>);

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        self.0.set(false);
    }
}

/// Spawns `fut` while keeping `guard` alive for the whole task, so the guard's `Drop`
/// releases its slot when the task completes, errors, or is cancelled by unmounting.
pub fn spawn_guarded<G, F>(guard: G, fut: F) -> dioxus::core::Task
where
    G: 'static,
    F: Future<Output = ()> + 'static,
{
    spawn(async move {
        let _held = guard;
        fut.await;
    })
}
