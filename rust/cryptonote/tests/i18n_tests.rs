#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote::messages::*;
use cryptonote::{AppError, Language, I18N, SUPPORTED_LANGUAGES};
use functora_dioxus::Msg as BaseMsg;

#[test]
fn supported_languages_contains_known() {
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Eng));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Spa));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Rus));
}

#[test]
fn supported_languages_have_flags_and_names() {
    SUPPORTED_LANGUAGES.iter().copied().for_each(|lang| {
        assert_ne!(BaseMsg::LanguageFlag(lang).render(Language::Eng), "🌐");
        assert!(!BaseMsg::LanguageName(lang).render(Language::Eng).is_empty());
    });
}

#[test]
fn unknown_language_falls_back() {
    assert_eq!(BaseMsg::LanguageFlag(Language::default()).render(Language::Eng), "🌐");
    assert_eq!(BaseMsg::LanguageFlag(Language::Fra).render(Language::Eng), "🌐");
    assert_eq!(
        BaseMsg::LanguageName(Language::default()).render(Language::Eng),
        "Unknown"
    );
}

#[test]
fn i18n_english_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Eng), "Note");
    assert_eq!(Msg::Share.render(Language::Eng), "Share");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Eng), "Back");
    assert_eq!(BaseMsg::Theme.render(Language::Eng), "Theme");
}

#[test]
fn i18n_spanish_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Spa), "Nota");
    assert_eq!(Msg::Share.render(Language::Spa), "Compartir");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Spa), "Atrás");
    assert_eq!(BaseMsg::Theme.render(Language::Spa), "Tema");
}

#[test]
fn i18n_russian_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Rus), "Заметка");
    assert_eq!(Msg::Share.render(Language::Rus), "Поделиться");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Rus), "Назад");
    assert_eq!(BaseMsg::Theme.render(Language::Rus), "Тема");
}

#[test]
fn i18n_unsupported_falls_back_to_english() {
    let unsupported = Language::Fra;
    assert_eq!(Msg::Note.render(unsupported), Msg::Note.render(Language::Eng));
    assert_eq!(Msg::Share.render(unsupported), Msg::Share.render(Language::Eng));
}

#[test]
fn i18n_all_messages_have_translations() {
    assert!(BaseMsg::LicenseText.render(Language::Eng).contains("Copyright"));
    assert!(BaseMsg::LicenseText.render(Language::Spa).contains("Copyright"));
    assert!(BaseMsg::LicenseText.render(Language::Rus).contains("Copyright"));
}

#[test]
fn i18n_all_supported_languages_render_differently() {
    let eng = Msg::Note.render(Language::Eng);
    let spa = Msg::Note.render(Language::Spa);
    let rus = Msg::Note.render(Language::Rus);
    assert_ne!(eng, spa);
    assert_ne!(eng, rus);
    assert_ne!(spa, rus);
}

#[test]
fn i18n_render_dispatches_correct_language() {
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Eng), "Home");
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Spa), "Inicio");
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Rus), "Главная");
}

#[test]
fn app_error_i18n_render_eng_non_empty() {
    let cases: Vec<AppError> = vec![
        AppError::PasswordRequired,
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::Archive("zip error".to_string()),
        AppError::InvalidFormat("nonce".into()),
        AppError::Json(serde_json::from_str::<serde_json::Value>("j").unwrap_err().into()),
        AppError::Utf8(String::from_utf8(vec![0xff]).unwrap_err()),
    ];
    for case in &cases {
        assert!(!case.render_eng().is_empty(), "render_eng empty for {case}");
    }
}

#[test]
fn app_error_i18n_render_spa_different_from_eng() {
    assert_ne!(
        AppError::PasswordRequired.render_eng(),
        AppError::PasswordRequired.render_spa()
    );
}

#[test]
fn app_error_i18n_render_rus_different_from_eng() {
    assert_ne!(
        AppError::PasswordRequired.render_eng(),
        AppError::PasswordRequired.render_rus()
    );
}

#[test]
fn app_error_i18n_archive_contains_detail() {
    let err = AppError::Archive("corrupt".to_string());
    assert!(err.render_eng().contains("corrupt"));
    assert!(err.render_spa().contains("corrupt"));
    assert!(err.render_rus().contains("corrupt"));
}

#[test]
fn app_error_i18n_invalid_format_contains_detail() {
    let err = AppError::InvalidFormat("nonce".into());
    assert!(err.render_eng().contains("nonce"));
    assert!(err.render_spa().contains("nonce"));
    assert!(err.render_rus().contains("nonce"));
}

#[test]
fn app_error_i18n_json_contains_detail() {
    let err = AppError::Json(serde_json::from_str::<serde_json::Value>("j").unwrap_err().into());
    let detail = serde_json::from_str::<serde_json::Value>("j").unwrap_err().to_string();
    assert!(err.render_eng().contains(&detail));
    assert!(err.render_spa().contains(&detail));
    assert!(err.render_rus().contains(&detail));
}

#[test]
fn app_error_i18n_utf8_contains_detail() {
    let err = AppError::Utf8(String::from_utf8(vec![0xff]).unwrap_err());
    assert!(err.render_eng().contains("UTF-8"));
    assert!(err.render_spa().contains("UTF-8"));
    assert!(err.render_rus().contains("UTF-8"));
}

#[test]
fn msg_i18n_note_label_variants() {
    assert_eq!(Msg::Note.render(Language::Eng), "Note");
    assert_eq!(Msg::Note.render(Language::Spa), "Nota");
    assert_eq!(Msg::Note.render(Language::Rus), "Заметка");
}

#[test]
fn msg_i18n_share_variants() {
    assert_eq!(Msg::Share.render(Language::Eng), "Share");
    assert_eq!(Msg::Share.render(Language::Spa), "Compartir");
    assert_eq!(Msg::Share.render(Language::Rus), "Поделиться");
}

#[test]
fn msg_i18n_footer_share_variants() {
    assert_eq!(BaseMsg::FooterShareWord.render(Language::Eng), "Share");
    assert_eq!(BaseMsg::FooterShareWord.render(Language::Spa), "Compartir");
    assert_eq!(BaseMsg::FooterShareWord.render(Language::Rus), "Поделитесь");
    assert_eq!(BaseMsg::FooterAppWord.render(Language::Eng), "app");
    assert_eq!(BaseMsg::FooterAppWord.render(Language::Spa), "la app");
    assert_eq!(BaseMsg::FooterAppWord.render(Language::Rus), "приложением");
}

#[test]
fn white_label_messages_render_all_languages() {
    let variants: Vec<BaseMsg> = vec![
        BaseMsg::Copyright,
        BaseMsg::AllRightsReserved,
        BaseMsg::ByContinuing,
        BaseMsg::YouAgree,
        BaseMsg::TermsOfService,
        BaseMsg::TermsOfServiceTitle,
        BaseMsg::PrivacyPolicyAnd,
        BaseMsg::PrivacyPolicyTitle,
        BaseMsg::Home,
        BaseMsg::VersionLabel,
        BaseMsg::Application,
        BaseMsg::Theme,
        BaseMsg::Donate,
        BaseMsg::DonateLink,
        BaseMsg::And,
        BaseMsg::FooterShareWord,
        BaseMsg::FooterAppWord,
        BaseMsg::LicenseText,
        BaseMsg::PrivacyText,
        BaseMsg::CopyAppLink,
        BaseMsg::ShareAppLink,
        BaseMsg::Sent,
        BaseMsg::SourceCodeButton,
        BaseMsg::AuthorButton,
        BaseMsg::JoinTestingButton,
        BaseMsg::GooglePlayButton,
        BaseMsg::DownloadApkButton,
        BaseMsg::AboutAndroidBeta1,
        BaseMsg::AboutAndroidBetaLink1,
        BaseMsg::AboutAndroidBeta2,
        BaseMsg::AboutAndroidBetaLink2,
        BaseMsg::AboutAndroidBeta3,
        BaseMsg::AboutAndroidBetaLink3,
        BaseMsg::AboutAndroidBeta4,
    ];
    for variant in &variants {
        let eng = variant.render(Language::Eng);
        assert!(!eng.is_empty(), "English empty for variant");
        let spa = variant.render(Language::Spa);
        assert!(!spa.is_empty(), "Spanish empty for variant");
        let rus = variant.render(Language::Rus);
        assert!(!rus.is_empty(), "Russian empty for variant");
    }
}

#[test]
fn msg_i18n_all_basic_variants_render_non_empty() {
    let variants: Vec<Msg> = vec![
        Msg::Note,
        Msg::NotePlaceholder,
        Msg::Mode,
        Msg::NoEncryption,
        Msg::EncryptionSuffix,
        Msg::Share,
        Msg::Sent,
        Msg::SharedNoteText,
        Msg::EncryptedNote,
        Msg::EncryptedNoteDesc,
        Msg::DecryptButton,
        Msg::CreateNewNote,
        Msg::EditNote,
        Msg::ViewButton,
        Msg::OpenUrlLabel,
        Msg::OpenUrlPlaceholder,
        Msg::OpenButton,
        Msg::ActionLabel,
        Msg::ActionCreate,
        Msg::ActionOpen,
        Msg::ActionScan,
        Msg::Print,
        Msg::Clear,
        Msg::AttachFiles,
        Msg::RemoveFile,
        Msg::ArchiveReady,
        Msg::OpenArchive,
        Msg::DownloadAll,
        Msg::FileName,
        Msg::FileSize,
    ];
    for variant in &variants {
        let eng = variant.render(Language::Eng);
        assert!(!eng.is_empty(), "English empty for variant");
        let spa = variant.render(Language::Spa);
        assert!(!spa.is_empty(), "Spanish empty for variant");
        let rus = variant.render(Language::Rus);
        assert!(!rus.is_empty(), "Russian empty for variant");
    }
}

#[test]
fn msg_i18n_error_delegates_to_app_error() {
    let msg = Msg::Error(AppError::PasswordRequired.into());
    assert_eq!(
        msg.render(Language::Eng),
        AppError::PasswordRequired.render(Language::Eng)
    );
    assert_eq!(
        msg.render(Language::Spa),
        AppError::PasswordRequired.render(Language::Spa)
    );
}

#[test]
fn app_error_i18n_qr_contains_detail() {
    let err = cryptonote::encoding::generate_qr_code("").unwrap_err();
    assert!(err.render_eng().contains("QR") || err.render_eng().contains("code"));
}

#[test]
fn supported_languages_length() {
    assert_eq!(SUPPORTED_LANGUAGES.len(), 3);
}

#[test]
fn language_flag_eng() {
    assert_eq!(BaseMsg::LanguageFlag(Language::Eng).render(Language::Eng), "🇬🇧");
    assert_eq!(BaseMsg::LanguageName(Language::Eng).render(Language::Eng), "English");
}

#[test]
fn language_flag_spa() {
    assert_eq!(BaseMsg::LanguageFlag(Language::Spa).render(Language::Spa), "🇪🇸");
    assert_eq!(BaseMsg::LanguageName(Language::Spa).render(Language::Spa), "Español");
}

#[test]
fn language_flag_rus() {
    assert_eq!(BaseMsg::LanguageFlag(Language::Rus).render(Language::Rus), "🇷🇺");
    assert_eq!(BaseMsg::LanguageName(Language::Rus).render(Language::Rus), "Русский");
}

#[test]
fn app_error_json_utf8_eng() {
    let json: AppError = serde_json::from_str::<serde_json::Value>("x").unwrap_err().into();
    assert!(!json.render_eng().is_empty());
    let utf8: AppError = String::from_utf8(vec![0xff]).unwrap_err().into();
    assert!(!utf8.render_eng().is_empty());
}

#[test]
fn app_error_fd_delegation_eng() {
    let err = AppError::FunctoraDioxus(functora_dioxus::Error::IO(
        std::io::Error::other("io".to_string()).into(),
    ));
    assert!(!err.render_eng().is_empty());
    assert!(err.render_eng().contains("io"));
}

#[test]
fn app_error_from_serde_json() {
    let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
    let err: AppError = json_err.into();
    assert!(matches!(err, AppError::Json(_)));
}

#[test]
fn app_error_i18n_render_spa_all_variants() {
    let variants: Vec<AppError> = vec![
        serde_json::from_str::<serde_json::Value>("x").unwrap_err().into(),
        String::from_utf8(vec![0xff]).unwrap_err().into(),
        cryptonote::AppError::FunctoraDioxus(cryptonote::encoding::generate_qr_code("").unwrap_err()),
        AppError::PasswordRequired,
        AppError::InvalidFormat("f".into()),
        AppError::Archive("a".to_string()),
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::FunctoraDioxus(functora_dioxus::Error::IO(
            std::io::Error::other("io".to_string()).into(),
        )),
    ];
    for v in &variants {
        let spa = v.render_spa();
        assert!(!spa.is_empty(), "render_spa empty for {v}");
        assert_ne!(v.render_eng(), spa, "render_spa same as eng for {v}");
    }
}

#[test]
fn app_error_i18n_render_rus_all_variants() {
    let variants: Vec<AppError> = vec![
        serde_json::from_str::<serde_json::Value>("x").unwrap_err().into(),
        String::from_utf8(vec![0xff]).unwrap_err().into(),
        cryptonote::AppError::FunctoraDioxus(cryptonote::encoding::generate_qr_code("").unwrap_err()),
        AppError::PasswordRequired,
        AppError::InvalidFormat("f".into()),
        AppError::Archive("a".to_string()),
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::FunctoraDioxus(functora_dioxus::Error::IO(
            std::io::Error::other("io".to_string()).into(),
        )),
    ];
    for v in &variants {
        let rus = v.render_rus();
        assert!(!rus.is_empty(), "render_rus empty for {v}");
        assert_ne!(v.render_eng(), rus, "render_rus same as eng for {v}");
    }
}

#[test]
fn msg_i18n_about_android_beta_all_languages() {
    assert!(BaseMsg::AboutAndroidBeta1.render(Language::Eng).contains("closed beta"));
    assert!(BaseMsg::AboutAndroidBeta1
        .render(Language::Spa)
        .contains("beta cerrada"));
    assert!(BaseMsg::AboutAndroidBeta1
        .render(Language::Rus)
        .contains("бета-тестирован"));
    assert!(BaseMsg::AboutAndroidBetaLink1
        .render(Language::Eng)
        .contains("closed beta"));
    assert!(BaseMsg::AboutAndroidBetaLink2
        .render(Language::Eng)
        .contains("Google Play"));
    assert!(BaseMsg::AboutAndroidBetaLink2
        .render(Language::Spa)
        .contains("Google Play"));
    assert!(BaseMsg::AboutAndroidBetaLink2
        .render(Language::Rus)
        .contains("Google Play"));
    assert!(BaseMsg::AboutAndroidBetaLink3.render(Language::Eng).contains("APK"));
    assert!(BaseMsg::AboutAndroidBetaLink3.render(Language::Spa).contains("APK"));
    assert!(BaseMsg::AboutAndroidBetaLink3.render(Language::Rus).contains("APK"));
    assert!(BaseMsg::AboutAndroidBeta4.render(Language::Eng).contains("directly"));
    assert!(BaseMsg::AboutAndroidBeta4
        .render(Language::Spa)
        .contains("directamente"));
    assert!(BaseMsg::AboutAndroidBeta4.render(Language::Rus).contains("напрямую"));
}

#[test]
fn msg_i18n_privacy_text_all_languages() {
    assert!(BaseMsg::PrivacyText.render(Language::Eng).contains("Privacy Policy"));
    assert!(BaseMsg::PrivacyText
        .render(Language::Spa)
        .contains("Política de Privacidad"));
    assert!(BaseMsg::PrivacyText
        .render(Language::Rus)
        .contains("Политика конфиденциальности"));
    assert!(BaseMsg::PrivacyText
        .render(Language::Eng)
        .contains("functora@proton.me"));
}

#[test]
fn msg_i18n_about_text_all_languages() {
    use cryptonote::messages::Msg;
    assert!(Msg::AboutText.render(Language::Eng).contains("Cryptonote"));
    assert!(Msg::AboutText.render(Language::Eng).contains("offline"));
    assert!(Msg::AboutText.render(Language::Spa).contains("Cryptonote"));
    assert!(Msg::AboutText.render(Language::Rus).contains("Cryptonote"));
}

#[test]
fn msg_i18n_error_delegates_to_app_error_spa() {
    use cryptonote::messages::Msg;
    let msg = Msg::Error(AppError::PasswordRequired.into());
    assert_eq!(
        msg.render(Language::Spa),
        AppError::PasswordRequired.render(Language::Spa)
    );
}

#[test]
fn msg_i18n_error_delegates_to_app_error_rus() {
    use cryptonote::messages::Msg;
    let msg = Msg::Error(AppError::PasswordRequired.into());
    assert_eq!(
        msg.render(Language::Rus),
        AppError::PasswordRequired.render(Language::Rus)
    );
}
