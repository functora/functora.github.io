use dioxus::document::Eval;
use serde_json::Value;

/// Sends `payload` to its `Eval` when dropped without `disarm`, so a streaming
/// eval script that is abandoned mid-stream (e.g. its component unmounted and
/// the runtime cancelled the task) is told to stop and free its accumulated
/// buffers instead of leaving them reachable in the `WebView` forever.
pub struct EvalAbort {
    eval: Eval,
    payload: Value,
    armed: bool,
}

impl EvalAbort {
    #[must_use]
    pub fn new(eval: Eval, payload: Value) -> Self {
        Self {
            eval,
            payload,
            armed: true,
        }
    }

    pub fn disarm(mut self) {
        self.armed = false;
    }
}

impl Drop for EvalAbort {
    fn drop(&mut self) {
        if self.armed
            && let Err(e) = self.eval.send(self.payload.clone())
        {
            tracing::warn!("Eval abort send failed: {e}");
        }
    }
}
