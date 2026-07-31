use cryptonote::messages::*;
use cryptonote::{language_label, AppError, Language, I18N, SUPPORTED_LANGUAGES};
use functora_dioxus::Msg as BaseMsg;

#[test]
fn supported_languages_contains_known() {
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Eng));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Spa));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Rus));
}

#[test]
fn supported_languages_matches_labels() {
    SUPPORTED_LANGUAGES
        .iter()
        .for_each(|lang| assert_ne!(language_label(*lang), "🌐 Unknown"));
}

#[test]
fn unknown_language_label_falls_back() {
    assert_eq!(language_label(Language::default()), "🌐 Unknown");
    assert_eq!(language_label(Language::Fra), "🌐 Unknown");
}

#[test]
fn i18n_english_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Eng), "Note");
    assert_eq!(Msg::Share.render(Language::Eng), "Share");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Eng), "Back");
    assert_eq!(Msg::Theme.render(Language::Eng), "Theme");
}

#[test]
fn i18n_spanish_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Spa), "Nota");
    assert_eq!(Msg::Share.render(Language::Spa), "Compartir");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Spa), "Atrás");
    assert_eq!(Msg::Theme.render(Language::Spa), "Tema");
}

#[test]
fn i18n_russian_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Rus), "Заметка");
    assert_eq!(Msg::Share.render(Language::Rus), "Поделиться");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Rus), "Назад");
    assert_eq!(Msg::Theme.render(Language::Rus), "Тема");
}

#[test]
fn i18n_unsupported_falls_back_to_english() {
    let unsupported = Language::Fra;
    assert_eq!(Msg::Note.render(unsupported), Msg::Note.render(Language::Eng));
    assert_eq!(Msg::Share.render(unsupported), Msg::Share.render(Language::Eng));
}

#[test]
fn i18n_all_messages_have_translations() {
    assert!(Msg::LicenseText.render(Language::Eng).contains("Copyright"));
    assert!(Msg::LicenseText.render(Language::Spa).contains("Copyright"));
    assert!(Msg::LicenseText.render(Language::Rus).contains("Copyright"));
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
        AppError::Archive("zip error".into()),
        AppError::Encrypt("fail".into()),
        AppError::Decrypt("fail".into()),
        AppError::KeyDerive("kdf".into()),
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
    let err = AppError::Archive("corrupt".into());
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
fn app_error_i18n_key_derive_contains_detail() {
    let err = AppError::KeyDerive("kdf".into());
    assert!(err.render_eng().contains("kdf"));
    assert!(err.render_spa().contains("kdf"));
    assert!(err.render_rus().contains("kdf"));
}

#[test]
fn app_error_from_getrandom() {
    let err: AppError = getrandom::Error::UNSUPPORTED.into();
    assert!(matches!(err, AppError::Getrandom(_)));
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
        Msg::Copyright,
        Msg::AllRightsReserved,
        Msg::ByContinuing,
        Msg::YouAgree,
        Msg::TermsOfService,
        Msg::TermsOfServiceTitle,
        Msg::PrivacyPolicyAnd,
        Msg::PrivacyPolicyTitle,
        Msg::VersionLabel,
        Msg::AboutTitle,
        Msg::JoinTestingButton,
        Msg::GooglePlayButton,
        Msg::DownloadApkButton,
        Msg::SourceCodeButton,
        Msg::AuthorButton,
        Msg::Donate,
        Msg::OpenUrlLabel,
        Msg::OpenUrlPlaceholder,
        Msg::OpenButton,
        Msg::DonateGreeting,
        Msg::DonateLink,
        Msg::Please,
        Msg::ActionLabel,
        Msg::ActionCreate,
        Msg::ActionOpen,
        Msg::ActionScan,
        Msg::Theme,
        Msg::ScanQrButton,
        Msg::QrScannerTitle,
        Msg::Print,
        Msg::Clear,
        Msg::AttachFiles,
        Msg::RemoveFile,
        Msg::ArchiveReady,
        Msg::OpenArchive,
        Msg::DownloadAll,
        Msg::ArchiveDecrypted,
        Msg::ExtractedNote,
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
    let msg = Msg::Error(AppError::PasswordRequired);
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
fn constants_are_defined() {
    assert!(!cryptonote::APP_NAME.is_empty());
    assert!(!cryptonote::APP_VERSION.is_empty());
    assert!(!cryptonote::APP_ID.is_empty());
    assert!(cryptonote::BETA_TEST_URL.starts_with("https://"));
    assert!(cryptonote::GOOGLE_PLAY_URL.starts_with("https://play.google.com"));
    assert!(cryptonote::APK_URL.starts_with("https://github.com"));
    assert!(cryptonote::WEB_APP_URL.starts_with("https://functora.github.io"));
    assert!(cryptonote::FUNCTORA_URL.starts_with("https://"));
    assert!(cryptonote::SOURCE_CODE_URL.starts_with("https://github.com"));
}

#[test]
fn supported_languages_length() {
    assert_eq!(SUPPORTED_LANGUAGES.len(), 3);
}

#[test]
fn language_label_eng() {
    assert!(language_label(Language::Eng).contains("English"));
}

#[test]
fn language_label_spa() {
    assert!(language_label(Language::Spa).contains("Español"));
}

#[test]
fn language_label_rus() {
    assert!(language_label(Language::Rus).contains("Русский"));
}

#[test]
fn app_error_cipher_getrandom_base64_json_utf8_eng() {
    let cipher: AppError = AppError::KeyDerive("cipher".into());
    assert!(!cipher.render_eng().is_empty());
    let getrandom: AppError = getrandom::Error::UNSUPPORTED.into();
    assert!(!getrandom.render_eng().is_empty());
    let base64: AppError = base64::DecodeError::InvalidLength(1).into();
    assert!(!base64.render_eng().is_empty());
    let json: AppError = serde_json::from_str::<serde_json::Value>("x").unwrap_err().into();
    assert!(!json.render_eng().is_empty());
    let utf8: AppError = String::from_utf8(vec![0xff]).unwrap_err().into();
    assert!(!utf8.render_eng().is_empty());
}

#[test]
fn app_error_fd_delegation_eng() {
    let err = AppError::FunctoraDioxus(functora_dioxus::Error::IO("io".into()));
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
        AppError::KeyDerive("kdf".into()),
        getrandom::Error::UNSUPPORTED.into(),
        base64::DecodeError::InvalidLength(1).into(),
        serde_json::from_str::<serde_json::Value>("x").unwrap_err().into(),
        String::from_utf8(vec![0xff]).unwrap_err().into(),
        cryptonote::encoding::generate_qr_code("").unwrap_err(),
        AppError::Encrypt("e".into()),
        AppError::Decrypt("d".into()),
        AppError::PasswordRequired,
        AppError::InvalidFormat("f".into()),
        AppError::Archive("a".into()),
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::FunctoraDioxus(functora_dioxus::Error::IO("io".into())),
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
        AppError::KeyDerive("kdf".into()),
        getrandom::Error::UNSUPPORTED.into(),
        base64::DecodeError::InvalidLength(1).into(),
        serde_json::from_str::<serde_json::Value>("x").unwrap_err().into(),
        String::from_utf8(vec![0xff]).unwrap_err().into(),
        cryptonote::encoding::generate_qr_code("").unwrap_err(),
        AppError::Encrypt("e".into()),
        AppError::Decrypt("d".into()),
        AppError::PasswordRequired,
        AppError::InvalidFormat("f".into()),
        AppError::Archive("a".into()),
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::FunctoraDioxus(functora_dioxus::Error::IO("io".into())),
    ];
    for v in &variants {
        let rus = v.render_rus();
        assert!(!rus.is_empty(), "render_rus empty for {v}");
        assert_ne!(v.render_eng(), rus, "render_rus same as eng for {v}");
    }
}

#[test]
fn msg_i18n_about_android_beta_all_languages() {
    use cryptonote::messages::Msg;
    assert!(Msg::AboutAndroidBeta1.render(Language::Eng).contains("closed beta"));
    assert!(Msg::AboutAndroidBeta1.render(Language::Spa).contains("beta cerrada"));
    assert!(Msg::AboutAndroidBeta1.render(Language::Rus).contains("бета-тестирован"));
    assert!(Msg::AboutAndroidBetaLink1.render(Language::Eng).contains("closed beta"));
    assert!(Msg::AboutAndroidBetaLink2.render(Language::Eng).contains("Google Play"));
    assert!(Msg::AboutAndroidBetaLink2.render(Language::Spa).contains("Google Play"));
    assert!(Msg::AboutAndroidBetaLink2.render(Language::Rus).contains("Google Play"));
    assert!(Msg::AboutAndroidBetaLink3.render(Language::Eng).contains("APK"));
    assert!(Msg::AboutAndroidBetaLink3.render(Language::Spa).contains("APK"));
    assert!(Msg::AboutAndroidBetaLink3.render(Language::Rus).contains("APK"));
    assert!(Msg::AboutAndroidBeta4.render(Language::Eng).contains("directly"));
    assert!(Msg::AboutAndroidBeta4.render(Language::Spa).contains("directamente"));
    assert!(Msg::AboutAndroidBeta4.render(Language::Rus).contains("напрямую"));
}

#[test]
fn msg_i18n_privacy_text_all_languages() {
    use cryptonote::messages::Msg;
    assert!(Msg::PrivacyText.render(Language::Eng).contains("Privacy Policy"));
    assert!(Msg::PrivacyText
        .render(Language::Spa)
        .contains("Política de Privacidad"));
    assert!(Msg::PrivacyText
        .render(Language::Rus)
        .contains("Политика конфиденциальности"));
    assert!(Msg::PrivacyText.render(Language::Eng).contains("functora@proton.me"));
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
fn msg_i18n_donate_intro_all_languages() {
    use cryptonote::messages::Msg;
    assert!(Msg::DonateIntro.render(Language::Eng).contains("Functora"));
    assert!(Msg::DonateIntro.render(Language::Spa).contains("Functora"));
    assert!(Msg::DonateIntro.render(Language::Rus).contains("Functora"));
}

#[test]
fn msg_i18n_error_delegates_to_app_error_spa() {
    use cryptonote::messages::Msg;
    let msg = Msg::Error(AppError::PasswordRequired);
    assert_eq!(
        msg.render(Language::Spa),
        AppError::PasswordRequired.render(Language::Spa)
    );
}

#[test]
fn msg_i18n_error_delegates_to_app_error_rus() {
    use cryptonote::messages::Msg;
    let msg = Msg::Error(AppError::PasswordRequired);
    assert_eq!(
        msg.render(Language::Rus),
        AppError::PasswordRequired.render(Language::Rus)
    );
}
