use functora_dioxus::i18n::I18N;

macro_rules! msg {
    ($name:ident, $eng:expr, $spa:expr, $rus:expr) => {
        #[derive(Clone, PartialEq)]
        pub struct $name;

        impl I18N for $name {
            fn render_eng(&self) -> String {
                $eng.to_string()
            }
            fn render_spa(&self) -> String {
                $spa.to_string()
            }
            fn render_rus(&self) -> String {
                $rus.to_string()
            }
        }
    };
}

msg!(MsgHome, "Home", "Inicio", "Главная");
msg!(MsgBack, "Back", "Atrás", "Назад");
msg!(MsgShareTitle, "Share", "Compartir", "Поделиться");
msg!(MsgEncryptedNote, "Encrypted", "Cifrado", "Шифр");
msg!(MsgYourNoteTitle, "Note", "Nota", "Заметка");
msg!(MsgErrorTitle, "Error", "Error", "Ошибка");
msg!(MsgDonateTitle, "Donate", "Donar", "Пожертвовать");
msg!(
    MsgPrivacyPolicyTitle,
    "Privacy Policy",
    "Política de Privacidad",
    "Политика конфиденциальности"
);
msg!(
    MsgTermsOfServiceTitle,
    "Terms of Service",
    "Términos de Servicio",
    "Условия обслуживания"
);
msg!(MsgAboutTitle, "About", "Referencia", "Справка");
