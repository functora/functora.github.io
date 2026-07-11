use functora_dioxus::{Error as FdError, Language, Theme, decode_qr_rgba, detect_browser_language, language_from_code};

#[test]
fn decode_qr_rgba_returns_none_for_zero_dimensions() {
    let data = vec![0u8; 16];
    let result = decode_qr_rgba(&data, 0, 0);
    assert!(result.is_none());
}

#[test]
fn decode_qr_rgba_returns_none_for_noise_data() {
    let mut data = vec![0u8; 400];
    for (i, b) in data.iter_mut().enumerate() {
        *b = (i % 256) as u8;
    }
    let result = decode_qr_rgba(&data, 10, 10);
    assert!(result.is_none());
}

#[test]
fn decode_qr_rgba_returns_none_for_small_data() {
    let data = vec![0u8; 16];
    let result = decode_qr_rgba(&data, 1, 1);
    assert!(result.is_none());
}

#[test]
fn decode_qr_rgba_handles_transparent_pixels() {
    let data = vec![0u8; 40];
    let result = decode_qr_rgba(&data, 2, 2);
    assert!(result.is_none());
}

#[test]
fn error_display() {
    let err: FdError = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found").into();
    let display = format!("{err}");
    assert!(display.contains("file not found"));
}

#[test]
fn error_io_from_io_error() {
    let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
    let err: FdError = io_err.into();
    assert!(matches!(err, FdError::IO(_)));
}

#[test]
fn error_json_from_json_error() {
    let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
    let err: FdError = json_err.into();
    assert!(matches!(err, FdError::Json(_)));
}

#[test]
fn error_env_from_env_var_error() {
    let env_err = std::env::VarError::NotPresent;
    let err: FdError = env_err.into();
    assert!(matches!(err, FdError::Env(_)));
}

#[test]
fn error_recv_from_recv_error() {
    let (tx, rx) = std::sync::mpsc::channel::<()>();
    drop(tx);
    let recv_err = rx.recv().unwrap_err();
    let err: FdError = recv_err.into();
    assert!(matches!(err, FdError::Channel(_)));
}

#[test]
fn theme_next_cycles() {
    assert_eq!(Theme::Light.next(), Theme::Dark);
    assert_eq!(Theme::Dark.next(), Theme::Light);
}

#[test]
fn theme_display() {
    assert_eq!(format!("{}", Theme::Light), "Light");
    assert_eq!(format!("{}", Theme::Dark), "Dark");
}

#[test]
fn theme_js_value() {
    assert_eq!(Theme::Light.to_js_value(), "light");
    assert_eq!(Theme::Dark.to_js_value(), "dark");
}

#[test]
fn error_camera_not_available() {
    let err = FdError::CameraNotAvailable("test".to_string());
    assert!(err.to_string().contains("test"));
}

#[test]
fn error_camera_permission_denied() {
    let err = FdError::CameraPermissionDenied("denied".to_string());
    assert!(err.to_string().contains("denied"));
}

#[test]
fn test_error_impl_std_error() {
    use std::error::Error;
    let err = FdError::NotJsonObject("test".to_string());
    assert!(err.source().is_none());
}

#[test]
fn test_error_source_chain() {
    use std::error::Error;
    let err: FdError = std::io::Error::new(std::io::ErrorKind::NotFound, "test").into();
    assert!(err.source().is_none());
}

#[test]
fn test_error_display_all_variants() {
    let variants: Vec<FdError> = vec![
        std::io::Error::new(std::io::ErrorKind::Other, "io").into(),
        serde_json::from_str::<serde_json::Value>("x").unwrap_err().into(),
        FdError::Env(std::env::VarError::NotPresent),
        FdError::Channel(std::sync::mpsc::RecvError),
        FdError::NotJsonObject("not object".to_string()),
    ];
    for v in variants {
        let _ = format!("{v}");
    }
}

#[test]
fn language_from_code_known_639_1() {
    assert_eq!(language_from_code("en"), Language::Eng);
    assert_eq!(language_from_code("es"), Language::Spa);
    assert_eq!(language_from_code("ru"), Language::Rus);
}

#[test]
fn language_from_code_strips_locale_region() {
    assert_eq!(language_from_code("en-US"), Language::Eng);
    assert_eq!(language_from_code("es-MX"), Language::Spa);
    assert_eq!(language_from_code("ru-RU"), Language::Rus);
}

#[test]
fn language_from_code_uppercase_known() {
    assert_eq!(language_from_code("EN"), Language::Eng);
    assert_eq!(language_from_code("ES"), Language::Spa);
}

#[test]
fn language_from_code_unknown_returns_default() {
    assert_eq!(language_from_code("zz"), Language::default());
    assert_eq!(language_from_code(""), Language::default());
    assert_eq!(language_from_code("xx-YY"), Language::default());
}

#[test]
fn detect_browser_language_does_not_panic() {
    let _ = detect_browser_language();
}
