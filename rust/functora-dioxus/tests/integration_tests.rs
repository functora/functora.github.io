use functora_dioxus::{Error as FdError, Language, Theme, decode_qr_rgba, detect_browser_language, language_from_code};
use std::fmt::Debug;

fn ok<T, E>(result: Result<T, E>) -> T
where
    E: Debug,
{
    match result {
        Ok(value) => value,
        Err(error) => panic!("expected Ok, got: {error:?}"),
    }
}

fn err<T, E>(result: Result<T, E>) -> E
where
    T: Debug,
{
    match result {
        Err(error) => error,
        Ok(value) => panic!("expected Err, got: {value:?}"),
    }
}

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
        *b = u8::try_from(i % 256).unwrap_or_default();
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
    let display = err.to_string();
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
    let json_err = err(serde_json::from_str::<serde_json::Value>("invalid"));
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
    let recv_err = err(rx.recv());
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
    let err = FdError::NotJsonObject(serde_json::json!("test"));
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
        std::io::Error::other("io").into(),
        err(serde_json::from_str::<serde_json::Value>("x")).into(),
        FdError::Env(std::env::VarError::NotPresent),
        FdError::Channel(std::sync::mpsc::RecvError),
        FdError::NotJsonObject(serde_json::json!("not object")),
    ];
    for v in variants {
        let _ = v.to_string();
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

#[test]
fn align_as_str_all_variants() {
    use functora_dioxus::Align;
    assert_eq!(Align::Left.as_str(), "l");
    assert_eq!(Align::Center.as_str(), "c");
    assert_eq!(Align::Right.as_str(), "r");
    assert_eq!(Align::Justify.as_str(), "j");
}

#[test]
fn align_serde_roundtrip() {
    use functora_dioxus::Align;
    for variant in &[Align::Left, Align::Center, Align::Right, Align::Justify] {
        let json = ok(serde_json::to_string(variant));
        let back: Align = ok(serde_json::from_str(&json));
        assert_eq!(*variant, back);
    }
}

#[test]
fn align_serde_lowercase_names() {
    use functora_dioxus::Align;
    assert_eq!(ok(serde_json::to_string(&Align::Left)), "\"left\"");
    assert_eq!(ok(serde_json::to_string(&Align::Center)), "\"center\"");
    assert_eq!(ok(serde_json::to_string(&Align::Right)), "\"right\"");
    assert_eq!(ok(serde_json::to_string(&Align::Justify)), "\"justify\"");
}

#[test]
fn theme_serde_roundtrip() {
    use functora_dioxus::Theme;
    for variant in &[Theme::Light, Theme::Dark] {
        let json = ok(serde_json::to_string(variant));
        let back: Theme = ok(serde_json::from_str(&json));
        assert_eq!(*variant, back);
    }
}

#[test]
fn frame_data_serde_roundtrip() {
    use functora_dioxus::FrameData;
    let f = FrameData {
        data: vec![1, 2, 3],
        width: 10,
        height: 20,
    };
    let json = ok(serde_json::to_string(&f));
    let back: FrameData = ok(serde_json::from_str(&json));
    assert_eq!(f.data, back.data);
    assert_eq!(f.width, back.width);
    assert_eq!(f.height, back.height);
}

#[test]
fn frame_data_serde_empty_data() {
    use functora_dioxus::FrameData;
    let f = FrameData {
        data: vec![],
        width: 0,
        height: 0,
    };
    let json = ok(serde_json::to_string(&f));
    let back: FrameData = ok(serde_json::from_str(&json));
    assert!(back.data.is_empty());
    assert_eq!(back.width, 0);
}

#[test]
fn share_data_serialize() {
    use functora_dioxus::ShareData;
    let s = ShareData {
        title: "t".into(),
        text: "msg".into(),
        url: "https://e.x".into(),
    };
    let json = ok(serde_json::to_string(&s));
    assert!(json.contains(r#""title":"t""#));
    assert!(json.contains(r#""text":"msg""#));
    assert!(json.contains(r#""url":"https://e.x""#));
}

#[test]
fn share_data_serialize_empty_fields() {
    use functora_dioxus::ShareData;
    let s = ShareData {
        title: String::new(),
        text: String::new(),
        url: String::new(),
    };
    let json = ok(serde_json::to_string(&s));
    assert!(json.contains(r#""title":"""#));
    assert!(json.contains(r#""text":"""#));
}

#[test]
fn decode_hints_has_try_harder() {
    let hints = functora_dioxus::qr::decode_hints();
    assert_eq!(hints.TryHarder, Some(true));
}

#[test]
fn decode_qr_luma_zero_width_returns_none() {
    assert!(functora_dioxus::qr::decode_qr_luma(&[], 0, 10).is_none());
    assert!(functora_dioxus::qr::decode_qr_luma(&[], 10, 0).is_none());
    assert!(functora_dioxus::qr::decode_qr_luma(&[], 0, 0).is_none());
}

#[test]
fn decode_qr_luma_noise_returns_none() {
    let noise: Vec<u8> = (0..10000)
        .map(|i| u8::try_from((i * 7919) % 256).unwrap_or_default())
        .collect();
    assert!(functora_dioxus::qr::decode_qr_luma(&noise, 100, 100).is_none());
}

#[test]
fn error_i18n_render_eng_non_empty() {
    use functora_dioxus::i18n::I18N;
    let cases: Vec<functora_dioxus::Error> = vec![
        std::io::Error::other("io").into(),
        err(serde_json::from_str::<serde_json::Value>("x")).into(),
        functora_dioxus::Error::Env(std::env::VarError::NotPresent),
        functora_dioxus::Error::Channel(std::sync::mpsc::RecvError),
        functora_dioxus::Error::JS("js error".to_string()),
        functora_dioxus::Error::CameraNotAvailable("cam".to_string()),
        functora_dioxus::Error::CameraPermissionDenied("denied".to_string()),
        functora_dioxus::Error::NotJsonObject("obj".into()),
    ];
    for case in &cases {
        assert!(!case.render_eng().is_empty(), "render_eng empty for {case}");
    }
}

#[test]
fn error_i18n_render_spa_different_from_eng() {
    use functora_dioxus::i18n::I18N;
    let err: functora_dioxus::Error = std::io::Error::other("test").into();
    assert_ne!(err.render_eng(), err.render_spa());
}

#[test]
fn error_i18n_render_rus_different_from_eng() {
    use functora_dioxus::i18n::I18N;
    let err = functora_dioxus::Error::NotJsonObject("x".into());
    assert_ne!(err.render_eng(), err.render_rus());
}

#[test]
fn error_i18n_js_contains_message() {
    use functora_dioxus::i18n::I18N;
    let err = functora_dioxus::Error::JS("something broke".to_string());
    assert!(err.render_eng().contains("something broke"));
}

#[test]
fn msg_i18n_render_eng_basic_variants() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let v: Vec<(Msg, &str)> = vec![
        (Msg::Copied, "Copied!"),
        (Msg::Password, "Password"),
        (Msg::Paste, "Paste"),
        (Msg::Copy, "Copy"),
        (Msg::Loading, "Loading..."),
        (Msg::ErrorTitleLabel, "Error"),
        (Msg::PasswordRequired, "Password is required for encryption"),
        (Msg::Back, "Back"),
        (Msg::Home, "Home"),
    ];
    for (msg, expected) in &v {
        assert_eq!(msg.render_eng(), *expected);
    }
}

#[test]
fn msg_i18n_render_spa_basic_variants() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert_eq!(Msg::Copied.render_spa(), "¡Copiado!");
    assert_eq!(Msg::Password.render_spa(), "Contraseña");
    assert_eq!(Msg::Back.render_spa(), "Atrás");
    assert_eq!(Msg::Home.render_spa(), "Inicio");
}

#[test]
fn msg_i18n_render_rus_basic_variants() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert_eq!(Msg::Copied.render_rus(), "Скопировано!");
    assert_eq!(Msg::Password.render_rus(), "Пароль");
    assert_eq!(Msg::Back.render_rus(), "Назад");
    assert_eq!(Msg::Home.render_rus(), "Главная");
}

#[test]
fn msg_i18n_error_title_contains_message() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let msg = Msg::ErrorTitle("disk full".into());
    assert!(msg.render_eng().contains("disk full"));
    assert!(msg.render_spa().contains("disk full"));
    assert!(msg.render_rus().contains("disk full"));
}

#[test]
fn msg_i18n_clipboard_errors_contain_message() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    for msg in &[
        Msg::ClipboardWriteError("perm".into()),
        Msg::ClipboardReadError("timeout".into()),
    ] {
        assert!(msg.render_eng().contains("perm") || msg.render_eng().contains("timeout"));
    }
}

#[test]
fn msg_i18n_camera_errors_contain_message() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let msg = Msg::CameraNotAvailable("no device".into());
    assert!(msg.render_eng().contains("no device"));
    let denied = Msg::CameraPermissionDenied("user denied".into());
    assert!(denied.render_eng().contains("user denied"));
}

#[test]
fn msg_i18n_env_error_contains_detail() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let msg = Msg::EnvError(std::env::VarError::NotPresent);
    assert!(msg.render_eng().contains("variable"));
}

#[test]
fn msg_i18n_channel_error_contains_detail() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let msg = Msg::ChannelError(std::sync::mpsc::RecvError);
    assert!(msg.render_eng().contains("channel"));
}

#[test]
fn msg_i18n_not_json_object_contains_detail() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    let msg = Msg::NotJsonObject("array".into());
    assert!(msg.render_eng().contains("array"));
}

#[test]
fn msg_i18n_eng_password_placeholder() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert_eq!(Msg::PasswordPlaceholder.render_eng(), "Enter password");
}

#[test]
fn msg_i18n_spa_remaining_basic() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert_eq!(Msg::PasswordPlaceholder.render_spa(), "Ingresa contraseña");
    assert_eq!(Msg::Paste.render_spa(), "Pegar");
    assert_eq!(Msg::Copy.render_spa(), "Copiar");
    assert_eq!(Msg::Loading.render_spa(), "Cargando...");
    assert_eq!(
        Msg::PasswordRequired.render_spa(),
        "Se requiere contraseña para el cifrado"
    );
    assert_eq!(Msg::Back.render_spa(), "Atrás");
    assert_eq!(Msg::Home.render_spa(), "Inicio");
}

#[test]
fn msg_i18n_rus_remaining_basic() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert_eq!(Msg::PasswordPlaceholder.render_rus(), "Введите пароль");
    assert_eq!(Msg::Paste.render_rus(), "Вставить");
    assert_eq!(Msg::Copy.render_rus(), "Копировать");
    assert_eq!(Msg::Loading.render_rus(), "Загрузка...");
    assert_eq!(Msg::PasswordRequired.render_rus(), "Для шифрования требуется пароль");
    assert_eq!(Msg::Back.render_rus(), "Назад");
    assert_eq!(Msg::Home.render_rus(), "Главная");
}

#[test]
fn msg_i18n_spa_error_variants() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert!(
        Msg::EnvError(std::env::VarError::NotPresent)
            .render_spa()
            .contains("variable")
    );
    let channel = Msg::ChannelError(std::sync::mpsc::RecvError);
    assert!(channel.render_spa().contains("canal") || channel.render_spa().contains("recepción"));
    assert!(Msg::NotJsonObject("arr".into()).render_spa().contains("JSON"));
    assert!(
        Msg::ClipboardWriteError("denied".into())
            .render_spa()
            .contains("denied")
    );
    assert!(
        Msg::ClipboardReadError("timeout".into())
            .render_spa()
            .contains("timeout")
    );
    assert!(Msg::CameraNotAvailable("none".into()).render_spa().contains("cámara"));
    assert!(
        Msg::CameraPermissionDenied("blocked".into())
            .render_spa()
            .contains("Permiso")
    );
    assert!(Msg::ErrorTitle("oops".into()).render_spa().contains("oops"));
}

#[test]
fn msg_i18n_rus_error_variants() {
    use functora_dioxus::Msg;
    use functora_dioxus::i18n::I18N;
    assert!(
        Msg::EnvError(std::env::VarError::NotPresent)
            .render_rus()
            .contains("переменной")
    );
    let channel = Msg::ChannelError(std::sync::mpsc::RecvError);
    assert!(channel.render_rus().contains("канала") || channel.render_rus().contains("получения"));
    assert!(Msg::NotJsonObject("arr".into()).render_rus().contains("JSON"));
    assert!(
        Msg::ClipboardWriteError("denied".into())
            .render_rus()
            .contains("denied")
    );
    assert!(
        Msg::ClipboardReadError("timeout".into())
            .render_rus()
            .contains("timeout")
    );
    assert!(Msg::CameraNotAvailable("none".into()).render_rus().contains("Камера"));
    assert!(
        Msg::CameraPermissionDenied("blocked".into())
            .render_rus()
            .contains("Разрешение")
    );
    assert!(Msg::ErrorTitle("oops".into()).render_rus().contains("oops"));
}

#[test]
fn error_i18n_spa_all_variants() {
    use functora_dioxus::Error;
    use functora_dioxus::i18n::I18N;
    let variants: Vec<Error> = vec![
        std::io::Error::other("io").into(),
        err(serde_json::from_str::<serde_json::Value>("x")).into(),
        Error::Env(std::env::VarError::NotPresent),
        Error::Channel(std::sync::mpsc::RecvError),
        Error::JS("js".to_string()),
        Error::CameraNotAvailable("cam".to_string()),
        Error::CameraPermissionDenied("denied".to_string()),
        Error::NotJsonObject("obj".into()),
    ];
    for v in &variants {
        let spa = v.render_spa();
        assert!(!spa.is_empty(), "render_spa empty for {v}");
        assert_ne!(v.render_eng(), spa, "render_spa same as eng for {v}");
    }
}

#[test]
fn error_i18n_rus_all_variants() {
    use functora_dioxus::Error;
    use functora_dioxus::i18n::I18N;
    let variants: Vec<Error> = vec![
        std::io::Error::other("io").into(),
        err(serde_json::from_str::<serde_json::Value>("x")).into(),
        Error::Env(std::env::VarError::NotPresent),
        Error::Channel(std::sync::mpsc::RecvError),
        Error::JS("js".to_string()),
        Error::CameraNotAvailable("cam".to_string()),
        Error::CameraPermissionDenied("denied".to_string()),
        Error::NotJsonObject("obj".into()),
    ];
    for v in &variants {
        let rus = v.render_rus();
        assert!(!rus.is_empty(), "render_rus empty for {v}");
        assert_ne!(v.render_eng(), rus, "render_rus same as eng for {v}");
    }
}
