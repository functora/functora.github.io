#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use functora_dioxus::Error;
use functora_dioxus::error::{IoError, JsonError, WorkerStopped, ZipErr};

fn io_err(kind: std::io::ErrorKind, msg: &str) -> std::io::Error {
    std::io::Error::new(kind, msg)
}

fn io_kind(err: &Error) -> std::io::ErrorKind {
    match err {
        Error::IO(e) => e.0.kind(),
        _ => std::io::ErrorKind::Other,
    }
}

fn io_os_code(err: &Error) -> Option<i32> {
    match err {
        Error::IO(e) => e.0.raw_os_error(),
        _ => None,
    }
}

fn json_props(err: &Error) -> (serde_json::error::Category, usize, usize) {
    match err {
        Error::Json(e) => (e.0.classify(), e.0.line(), e.0.column()),
        _ => (serde_json::error::Category::Eof, 0, 0),
    }
}

fn zip_msg(err: &Error) -> &'static str {
    match err {
        Error::Archive(e) => match e.0.as_ref() {
            zip::result::ZipError::InvalidArchive(s) | zip::result::ZipError::UnsupportedArchive(s) => s,
            _ => "",
        },
        _ => "",
    }
}

fn zip_io_kind(err: &Error) -> std::io::ErrorKind {
    match err {
        Error::Archive(e) => match e.0.as_ref() {
            zip::result::ZipError::Io(io) => io.kind(),
            _ => std::io::ErrorKind::Other,
        },
        _ => std::io::ErrorKind::Other,
    }
}

#[test]
fn io_errors_are_equal_only_when_sharing_an_instance() {
    let e = Arc::new(io_err(std::io::ErrorKind::NotFound, "a"));
    let a = Error::IO(IoError(e.clone()));
    let b = Error::IO(IoError(e));
    assert_eq!(a, b);
    let c = Error::IO(IoError(Arc::new(io_err(std::io::ErrorKind::NotFound, "a"))));
    assert_ne!(a, c);
}

#[test]
fn io_errors_preserve_kind_and_os_code() {
    let a = Error::IO(IoError::from(io_err(std::io::ErrorKind::NotFound, "a")));
    let b = Error::IO(IoError::from(std::io::Error::from_raw_os_error(2)));
    assert_eq!(io_kind(&a), std::io::ErrorKind::NotFound);
    assert_eq!(io_os_code(&b), Some(2));
}

#[test]
fn io_error_from_io_error_chains_into_wrapped_variant() {
    let err: Error = io_err(std::io::ErrorKind::NotFound, "test").into();
    assert!(matches!(err, Error::IO(_)));
}

#[test]
fn json_errors_are_equal_only_when_sharing_an_instance() {
    let e = Arc::new(serde_json::from_str::<serde_json::Value>("{").unwrap_err());
    let a = Error::Json(JsonError(e.clone()));
    let b = Error::Json(JsonError(e));
    assert_eq!(a, b);
    let c = Error::Json(JsonError(Arc::new(
        serde_json::from_str::<serde_json::Value>("{").unwrap_err(),
    )));
    assert_ne!(a, c);
}

#[test]
fn json_errors_preserve_class_and_position() {
    let a = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("x").unwrap_err(),
    ));
    let raw_a = serde_json::from_str::<serde_json::Value>("x").unwrap_err();
    let b = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("{").unwrap_err(),
    ));
    let raw_b = serde_json::from_str::<serde_json::Value>("{").unwrap_err();
    assert_eq!(json_props(&a), (raw_a.classify(), raw_a.line(), raw_a.column()));
    assert_eq!(json_props(&b), (raw_b.classify(), raw_b.line(), raw_b.column()));
    assert_ne!(json_props(&a), json_props(&b));
}

#[test]
fn json_errors_are_unequal_to_other_variants() {
    let json = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("{").unwrap_err(),
    ));
    let b64 = Error::Base64(base64::DecodeError::InvalidLength(3));
    assert_ne!(std::mem::discriminant(&json), std::mem::discriminant(&b64));
}

#[test]
fn zip_errors_are_equal_only_when_sharing_an_instance() {
    let e = Arc::new(zip::result::ZipError::InvalidArchive("boom"));
    let a = Error::Archive(ZipErr(e.clone()));
    let b = Error::Archive(ZipErr(e));
    assert_eq!(a, b);
    let c = Error::Archive(ZipErr(Arc::new(zip::result::ZipError::InvalidArchive("boom"))));
    assert_ne!(a, c);
}

#[test]
fn zip_errors_preserve_payload_and_io_kind() {
    let a = Error::Archive(ZipErr::from(zip::result::ZipError::InvalidArchive("one")));
    let b = Error::Archive(ZipErr::from(zip::result::ZipError::InvalidArchive("two")));
    let c = Error::Archive(ZipErr::from(zip::result::ZipError::Io(io_err(
        std::io::ErrorKind::NotFound,
        "a",
    ))));
    let d = Error::Archive(ZipErr::from(zip::result::ZipError::Io(io_err(
        std::io::ErrorKind::InvalidData,
        "x",
    ))));
    assert_eq!(zip_msg(&a), "one");
    assert_ne!(zip_msg(&a), zip_msg(&b));
    assert_eq!(zip_io_kind(&c), std::io::ErrorKind::NotFound);
    assert_ne!(zip_io_kind(&c), zip_io_kind(&d));
}

#[test]
fn error_derives_standard_traits() {
    let a = Error::IO(IoError::from(io_err(std::io::ErrorKind::Other, "x")));
    assert!(format!("{a:?}").contains("IO"));
    assert_eq!(a, a);
}

#[test]
fn worker_stopped_unit_error() {
    let a = Error::Worker(WorkerStopped);
    let b = Error::Worker(WorkerStopped);
    assert_eq!(a, b);
    assert_eq!(
        a.to_string(),
        "Background task error: Background task stopped unexpectedly"
    );
}

#[test]
fn eval_error_finished_maps_to_eval_finished_variant() {
    use dioxus_document::EvalError;
    let err: Error = EvalError::Finished.into();
    assert!(matches!(err, Error::EvalFinished));
    assert_ne!(err.to_string(), "");
}

#[test]
fn eval_error_communication_maps_to_js_string() {
    use dioxus_document::EvalError;
    let err: Error = EvalError::Communication("closed".into()).into();
    assert!(matches!(err, Error::JS(s) if s == "closed"));
}
