use functora_egui::i18n::{I18N, Language};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Msg {
    AccountRequired,
    ConversationCreated,
    MessageSent,
    InvitationAccepted,
    VaultLoaded,
    Error,
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::AccountRequired => "Create an account to continue".to_owned(),
            Self::ConversationCreated => "Conversation created".to_owned(),
            Self::MessageSent => "Message sent".to_owned(),
            Self::InvitationAccepted => "Invitation accepted".to_owned(),
            Self::VaultLoaded => "Vault loaded".to_owned(),
            Self::Error => "Something went wrong".to_owned(),
        }
    }

    fn render(&self, language: Language) -> String {
        match (self, language) {
            (Self::AccountRequired, Language::Spa) => "Crea una cuenta para continuar".to_owned(),
            (Self::AccountRequired, Language::Rus) => "Создайте аккаунт для продолжения".to_owned(),
            (Self::AccountRequired, _) => "Create an account to continue".to_owned(),
            (Self::ConversationCreated, Language::Spa) => "Conversación creada".to_owned(),
            (Self::ConversationCreated, Language::Rus) => "Беседа создана".to_owned(),
            (Self::ConversationCreated, _) => "Conversation created".to_owned(),
            (Self::MessageSent, Language::Spa) => "Mensaje enviado".to_owned(),
            (Self::MessageSent, Language::Rus) => "Сообщение отправлено".to_owned(),
            (Self::MessageSent, _) => "Message sent".to_owned(),
            (Self::InvitationAccepted, Language::Spa) => "Invitación aceptada".to_owned(),
            (Self::InvitationAccepted, Language::Rus) => "Приглашение принято".to_owned(),
            (Self::InvitationAccepted, _) => "Invitation accepted".to_owned(),
            (Self::VaultLoaded, Language::Spa) => "Bóveda cargada".to_owned(),
            (Self::VaultLoaded, Language::Rus) => "Хранилище загружено".to_owned(),
            (Self::VaultLoaded, _) => "Vault loaded".to_owned(),
            (Self::Error, Language::Spa) => "Se produjo un error".to_owned(),
            (Self::Error, Language::Rus) => "Произошла ошибка".to_owned(),
            (Self::Error, _) => "Something went wrong".to_owned(),
        }
    }
}
