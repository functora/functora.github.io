pub use functora_core::error::*;

#[must_use]
pub fn eval_error(e: dioxus::document::EvalError) -> Error {
    match e {
        dioxus::document::EvalError::Finished => Error::EvalFinished,
        dioxus::document::EvalError::InvalidJs(js) => Error::JS(js),
        dioxus::document::EvalError::Communication(msg) => Error::JS(msg),
        dioxus::document::EvalError::Serialization(err) => Error::Json(JsonError::from(err)),
        other => Error::JS(other.to_string()),
    }
}
