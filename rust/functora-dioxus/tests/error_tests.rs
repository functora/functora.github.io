#![allow(clippy::unwrap_used, clippy::expect_used)]

use functora_dioxus::Error;
use functora_dioxus::error::WorkerStopped;

fn io_err(kind: std::io::ErrorKind, msg: &str) -> std::io::Error {
    std::io::Error::new(kind, msg)
}

#[test]
fn io_errors_of_same_kind_are_equal() {
    let a = Error::IO(io_err(std::io::ErrorKind::NotFound, "a").into());
    let b = Error::IO(io_err(std::io::ErrorKind::NotFound, "b").into());
    assert_eq!(a, b);
}

#[test]
fn io_errors_of_different_kinds_are_unequal() {
    let a = Error::IO(io_err(std::io::ErrorKind::NotFound, "x").into());
    let b = Error::IO(io_err(std::io::ErrorKind::PermissionDenied, "x").into());
    assert_ne!(a, b);
}

#[test]
fn io_error_from_io_error_chains_into_wrapped_variant() {
    let err: Error = io_err(std::io::ErrorKind::NotFound, "test").into();
    assert!(matches!(err, Error::IO(_)));
}

#[test]
fn json_errors_of_same_class_are_equal() {
    let a = Error::Json(serde_json::from_str::<serde_json::Value>("{").unwrap_err().into());
    let b = Error::Json(serde_json::from_str::<serde_json::Value>("[").unwrap_err().into());
    assert_eq!(a, b);
}

#[test]
fn json_errors_are_unequal_to_other_variants() {
    let json = Error::Json(serde_json::from_str::<serde_json::Value>("{").unwrap_err().into());
    let b64 = Error::Base64(base64::DecodeError::InvalidLength(3));
    assert_ne!(json, b64);
}

#[test]
fn zip_errors_with_same_payload_are_equal() {
    let a = Error::Archive(zip::result::ZipError::InvalidArchive("boom").into());
    let b = Error::Archive(zip::result::ZipError::InvalidArchive("boom").into());
    assert_eq!(a, b);
}

#[test]
fn zip_errors_with_different_payloads_are_unequal() {
    let a = Error::Archive(zip::result::ZipError::InvalidArchive("one").into());
    let b = Error::Archive(zip::result::ZipError::InvalidArchive("two").into());
    assert_ne!(a, b);
}

#[test]
fn zip_io_errors_follow_io_kind_rules() {
    let a = Error::Archive(zip::result::ZipError::Io(io_err(std::io::ErrorKind::NotFound, "a")).into());
    let b = Error::Archive(zip::result::ZipError::Io(io_err(std::io::ErrorKind::NotFound, "b")).into());
    let c = Error::Archive(zip::result::ZipError::Io(io_err(std::io::ErrorKind::InvalidData, "x")).into());
    assert_eq!(a, b);
    assert_ne!(a, c);
}

#[test]
fn zip_unit_variants_equal_their_own_and_differ_from_others() {
    let a = Error::Archive(zip::result::ZipError::FileNotFound.into());
    let b = Error::Archive(zip::result::ZipError::FileNotFound.into());
    let c = Error::Archive(zip::result::ZipError::InvalidPassword.into());
    assert_eq!(a, b);
    assert_ne!(a, c);
}

#[test]
fn error_derives_standard_traits() {
    let a = Error::IO(io_err(std::io::ErrorKind::Other, "x").into());
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
