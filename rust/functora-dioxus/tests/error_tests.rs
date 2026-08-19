#![allow(clippy::unwrap_used, clippy::expect_used)]

use dioxus_document::EvalError;
use functora_dioxus::Error;
use functora_dioxus::error::eval_error;

#[test]
fn eval_error_finished_maps_to_eval_finished_variant() {
    let err: Error = eval_error(EvalError::Finished);
    assert!(matches!(err, Error::EvalFinished));
    assert_ne!(err.to_string(), "");
}

#[test]
fn eval_error_communication_maps_to_js_string() {
    let err: Error = eval_error(EvalError::Communication("closed".into()));
    assert!(matches!(err, Error::JS(s) if s == "closed"));
}
