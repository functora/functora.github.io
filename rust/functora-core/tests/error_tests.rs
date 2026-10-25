use std::sync::Arc;

use functora_core::Error;
#[cfg(feature = "zip")]
use functora_core::error::ZipErr;
use functora_core::error::{IoError, JsonError, WorkerStopped};

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

#[cfg(feature = "zip")]
fn zip_msg(err: &Error) -> &'static str {
    match err {
        Error::Archive(e) => match e.0.as_ref() {
            zip::result::ZipError::InvalidArchive(s)
            | zip::result::ZipError::UnsupportedArchive(s) => s,
            _ => "",
        },
        _ => "",
    }
}

#[cfg(feature = "zip")]
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
    let e = Arc::new(
        serde_json::from_str::<serde_json::Value>("{")
            .err()
            .unwrap_or_else(|| panic!("malformed json must fail to parse")),
    );
    let a = Error::Json(JsonError(e.clone()));
    let b = Error::Json(JsonError(e));
    assert_eq!(a, b);
    let c = Error::Json(JsonError(Arc::new(
        serde_json::from_str::<serde_json::Value>("{")
            .err()
            .unwrap_or_else(|| panic!("malformed json must fail to parse")),
    )));
    assert_ne!(a, c);
}

#[test]
fn json_errors_preserve_class_and_position() {
    let a = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("x")
            .err()
            .unwrap_or_else(|| panic!("malformed json must fail to parse")),
    ));
    let raw_a = serde_json::from_str::<serde_json::Value>("x")
        .err()
        .unwrap_or_else(|| panic!("malformed json must fail to parse"));
    let b = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("{")
            .err()
            .unwrap_or_else(|| panic!("malformed json must fail to parse")),
    ));
    let raw_b = serde_json::from_str::<serde_json::Value>("{")
        .err()
        .unwrap_or_else(|| panic!("malformed json must fail to parse"));
    assert_eq!(
        json_props(&a),
        (raw_a.classify(), raw_a.line(), raw_a.column())
    );
    assert_eq!(
        json_props(&b),
        (raw_b.classify(), raw_b.line(), raw_b.column())
    );
    assert_ne!(json_props(&a), json_props(&b));
}

#[test]
fn json_errors_are_unequal_to_other_variants() {
    let json = Error::Json(JsonError::from(
        serde_json::from_str::<serde_json::Value>("{")
            .err()
            .unwrap_or_else(|| panic!("malformed json must fail to parse")),
    ));
    let b64 = Error::Base64(base64::DecodeError::InvalidLength(3));
    assert_ne!(std::mem::discriminant(&json), std::mem::discriminant(&b64));
}

#[test]
#[cfg(feature = "zip")]
fn zip_errors_are_equal_only_when_sharing_an_instance() {
    let e = Arc::new(zip::result::ZipError::InvalidArchive("boom"));
    let a = Error::Archive(ZipErr(e.clone()));
    let b = Error::Archive(ZipErr(e));
    assert_eq!(a, b);
    let c = Error::Archive(ZipErr(Arc::new(zip::result::ZipError::InvalidArchive(
        "boom",
    ))));
    assert_ne!(a, c);
}

#[test]
#[cfg(feature = "zip")]
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
fn qr_not_found_renders_all_languages() {
    use functora_core::i18n::{I18N, Language};

    assert_eq!(
        Error::QrNotFound.render(Language::Eng),
        "No QR found in image"
    );
    assert_eq!(
        Error::QrNotFound.render(Language::Spa),
        "No se encontró ningún QR en la imagen"
    );
    assert_eq!(
        Error::QrNotFound.render(Language::Rus),
        "QR-код не найден на изображении"
    );
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
fn missing_objects_render_in_every_supported_language() {
    use functora_core::error::Missing;
    use functora_core::i18n::{I18N, SUPPORTED_LANGUAGES};

    let objects = [
        Missing::Window,
        Missing::Document,
        Missing::Body,
        Missing::VideoElement,
        Missing::Canvas,
        Missing::Context2d,
        Missing::LocalStorage,
        Missing::Anchor,
        Missing::Input,
        Missing::FileEntry,
        Missing::ClipboardText,
        Missing::DesktopSession,
        Missing::AndroidApp,
        Missing::DataDir,
    ];
    for object in objects {
        for lang in SUPPORTED_LANGUAGES.iter().copied() {
            let rendered = Error::Missing(object).render(lang);
            assert!(!rendered.is_empty(), "{object:?} rendered empty text");
        }
    }
}

#[test]
fn platform_failures_never_render_as_javascript_evaluation_errors() {
    use functora_core::error::{Capability, ClipboardError, Missing};
    use functora_core::i18n::{I18N, SUPPORTED_LANGUAGES};

    let failures = [
        Error::Missing(Missing::Window),
        Error::CapabilityDisabled(Capability::Clipboard),
        Error::CapabilityDisabled(Capability::Share),
        Error::CapabilityDisabled(Capability::Save),
        Error::Clipboard(ClipboardError::Unavailable),
        Error::Clipboard(ClipboardError::Busy),
        Error::Clipboard(ClipboardError::ContentUnavailable),
        Error::Clipboard(ClipboardError::ConversionFailed),
        Error::Clipboard(ClipboardError::Unknown("detail".into())),
        Error::FilePickerTimedOut,
        Error::FilePickerFailed,
        Error::SaveCancelled,
        Error::OpenUrlFailed {
            url: "https://example.com".into(),
        },
        Error::OpenUrlTimedOut {
            url: "https://example.com".into(),
        },
        Error::Worker(WorkerStopped),
    ];
    for failure in &failures {
        for lang in SUPPORTED_LANGUAGES.iter().copied() {
            let rendered = failure.render(lang).to_lowercase();
            assert!(
                !rendered.contains("javascript"),
                "mislabelled as a JS evaluation error: {rendered}"
            );
        }
    }
}

#[test]
fn platform_failures_render_precise_messages() {
    use functora_core::error::{Capability, ClipboardError, Missing};
    use functora_core::i18n::I18N;

    assert_eq!(
        Error::Missing(Missing::Window).render_eng(),
        "Required object unavailable: window"
    );
    assert_eq!(
        Error::Missing(Missing::DataDir).render_spa(),
        "Objeto requerido no disponible: data directory"
    );
    assert_eq!(
        Error::Missing(Missing::AndroidApp).render_rus(),
        "Требуемый объект недоступен: Android application"
    );
    assert_eq!(
        Error::CapabilityDisabled(Capability::Save).render_eng(),
        "save is unavailable: web feature disabled"
    );
    assert_eq!(
        Error::Clipboard(ClipboardError::Unknown("detail".to_string())).render_eng(),
        "Clipboard error: detail"
    );
    assert_eq!(
        Error::OpenUrlTimedOut {
            url: "https://example.com".to_string(),
        }
        .render_eng(),
        "xdg-open timed out for https://example.com"
    );
    assert_eq!(Error::SaveCancelled.to_string(), "Save was cancelled");
    assert_eq!(
        Error::FilePickerTimedOut.to_string(),
        "File picker timed out"
    );
    assert_eq!(Error::FilePickerFailed.to_string(), "File picker failed");
}
