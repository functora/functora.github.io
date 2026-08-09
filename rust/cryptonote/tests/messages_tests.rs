use cryptonote::messages::Msg;
use cryptonote::{AppError, Language, MsgError};
use functora_dioxus::i18n::I18N;

#[test]
fn unit_variants_are_equal_by_value() {
    assert_eq!(Msg::Note, Msg::Note);
    assert_ne!(Msg::Note, Msg::Share);
}

#[test]
fn language_variants_distinguish_flag_and_name() {
    assert_eq!(Msg::LanguageFlag(Language::Eng), Msg::LanguageFlag(Language::Eng));
    assert_eq!(Msg::LanguageName(Language::Eng), Msg::LanguageName(Language::Eng));
    assert_ne!(Msg::LanguageFlag(Language::Eng), Msg::LanguageName(Language::Eng));
    assert_ne!(Msg::LanguageFlag(Language::Eng), Msg::LanguageFlag(Language::Spa));
}

#[test]
fn downloaded_compares_payload() {
    assert_eq!(Msg::Downloaded("a".into()), Msg::Downloaded("a".into()));
    assert_ne!(Msg::Downloaded("a".into()), Msg::Downloaded("b".into()));
}

#[test]
fn base_and_app_errors_compare_by_value() {
    let a = Msg::Base(functora_dioxus::Msg::Back);
    let b = Msg::Base(functora_dioxus::Msg::Back);
    assert_eq!(a, b);
}

fn app_err(kind: std::io::ErrorKind) -> AppError {
    AppError::FunctoraDioxus(functora_dioxus::Error::IO(std::io::Error::new(kind, "x").into()))
}

#[test]
fn error_messages_are_equal_only_when_sharing_the_instance() {
    let a = Msg::Error(app_err(std::io::ErrorKind::NotFound).into());
    let b = a.clone();
    assert_eq!(a, b);
    let c = Msg::Error(app_err(std::io::ErrorKind::NotFound).into());
    assert_ne!(a, c);
}

#[test]
fn error_messages_compare_by_instance_identity() {
    let a = Msg::Error(MsgError::from(app_err(std::io::ErrorKind::NotFound)));
    let b = Msg::Error(MsgError::from(app_err(std::io::ErrorKind::NotFound)));
    let c = Msg::Error(MsgError::from(app_err(std::io::ErrorKind::InvalidData)));
    assert_eq!(a.clone(), a);
    assert_ne!(a, b);
    assert_ne!(a, c);
}

#[test]
fn msg_derives_debug() {
    assert!(!format!("{:?}", Msg::Sent).is_empty());
}

#[test]
fn msg_error_from_app_error_and_clone() {
    let msg = MsgError::from(AppError::PasswordRequired);
    let cloned = msg.clone();
    assert_eq!(msg, cloned);
    assert_eq!(Msg::Error(msg), Msg::Error(cloned));
}

#[test]
fn msg_error_derefs_to_app_error() {
    let msg = MsgError::from(AppError::PasswordRequired);
    assert_eq!(msg.render_eng(), "Password is required");
    assert_eq!(Msg::Error(msg).render_eng(), "Password is required");
}
