//! Every `Msg` variant must carry non-empty EN/ES/RU translations.

use cryptonote_egui::error::AppError;
use cryptonote_egui::messages::Msg;
use functora_egui::i18n::{I18N, Language, SUPPORTED_LANGUAGES};
use functora_egui::messages::Msg as BaseMsg;
use strum::EnumCount;

fn msg_all() -> Vec<Msg> {
    vec![
        Msg::Base(BaseMsg::Theme),
        Msg::Error(AppError::PasswordRequired.into()),
        Msg::Note,
        Msg::NotePlaceholder,
        Msg::Mode,
        Msg::NoEncryption,
        Msg::EncryptionSuffix,
        Msg::Share,
        Msg::Sent,
        Msg::SharedNoteText,
        Msg::ShareAppDesc,
        Msg::EncryptedNote,
        Msg::EncryptedNoteDesc,
        Msg::DecryptButton,
        Msg::EditNote,
        Msg::Reset,
        Msg::ResetDone,
        Msg::ViewButton,
        Msg::OpenUrlLabel,
        Msg::OpenUrlPlaceholder,
        Msg::OpenButton,
        Msg::ActionLabel,
        Msg::ActionCreate,
        Msg::ActionOpen,
        Msg::ActionScan,
        Msg::AboutText,
        Msg::Clear,
        Msg::AttachFiles,
        Msg::RemoveFile,
        Msg::ArchiveReady,
        Msg::OpenArchive,
        Msg::Download,
        Msg::DownloadAll,
        Msg::File,
        Msg::PreviewUnavailable,
        Msg::FileNotFound,
        Msg::Downloaded("note.txt".to_owned()),
    ]
}

#[test]
fn every_msg_variant_has_translations() {
    assert_eq!(msg_all().len(), Msg::COUNT, "msg_all must list every Msg variant");
    for variant in msg_all() {
        for lang in [Language::Eng, Language::Spa, Language::Rus] {
            assert!(!variant.render(lang).is_empty(), "empty translation for {variant:?}");
        }
    }
}

#[test]
fn supported_languages_have_flags_and_names() {
    assert_eq!(SUPPORTED_LANGUAGES.len(), 3);
    for lang in SUPPORTED_LANGUAGES.iter().copied() {
        assert_ne!(BaseMsg::LanguageFlag(lang).render(lang), "\u{1f310}");
        assert!(!BaseMsg::LanguageName(lang).render(lang).is_empty());
    }
}

#[test]
fn basic_messages_dispatch_all_languages() {
    assert_eq!(Msg::Note.render(Language::Eng), "Note");
    assert_eq!(Msg::Note.render(Language::Spa), "Nota");
    assert_eq!(
        Msg::Note.render(Language::Rus),
        "\u{417}\u{430}\u{43c}\u{435}\u{442}\u{43a}\u{430}"
    );
    assert_ne!(Msg::Share.render(Language::Eng), Msg::Share.render(Language::Spa));
    assert_ne!(Msg::Share.render(Language::Eng), Msg::Share.render(Language::Rus));
}

#[test]
fn error_delegates_to_app_error() {
    let msg = Msg::Error(AppError::PasswordRequired.into());
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_eq!(
            msg.render(lang),
            AppError::PasswordRequired.render(lang),
            "delegated error text must match AppError for {lang:?}"
        );
    }
}

#[test]
fn downloaded_message_embeds_location() {
    let rendered = Msg::Downloaded("/tmp/note.txt".to_owned()).render(Language::Eng);
    assert!(rendered.contains("/tmp/note.txt"));
}

#[test]
fn note_url_errors_are_localized() {
    for make_err in [AppError::NoNoteInUrl, AppError::NoNoteParam] {
        let eng = make_err.render(Language::Eng);
        let spa = make_err.render(Language::Spa);
        let rus = make_err.render(Language::Rus);
        assert!(!eng.is_empty());
        assert!(!spa.is_empty());
        assert!(!rus.is_empty());
        assert_ne!(eng, spa);
        assert_ne!(eng, rus);
    }
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_eq!(
            Msg::Error(AppError::NoNoteInUrl.into()).render(lang),
            AppError::NoNoteInUrl.render(lang),
        );
        assert_eq!(
            Msg::Error(AppError::NoNoteParam.into()).render(lang),
            AppError::NoNoteParam.render(lang),
        );
    }
}
