use functora_core::Msg;
use functora_core::i18n::{I18N, Language};

#[test]
fn pick_feedback_renders_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(!Msg::PickingFiles.render(lang).is_empty());
        let attached = Msg::FilesAttached(2).render(lang);
        assert!(attached.contains('2'), "count must render: {attached}");
        assert!(!attached.is_empty());
    }
}

#[test]
fn privacy_subheadings_render_as_headings() {
    let eng = [
        "## Privacy Policy",
        "## What information does the Application obtain and how is it used?",
        "## Does the Application collect precise real time location information of the device?",
        "## Do third parties see and/or have access to information obtained by the Application?",
        "## What are my opt-out rights?",
        "## Children",
        "## Security",
        "## Changes",
        "## Your Consent",
        "## Contact Us",
    ];
    let spa = [
        "## Política de Privacidad",
        "## ¿Qué información obtiene la Aplicación y cómo se utiliza?",
        "## ¿La Aplicación recopila información precisa de ubicación en tiempo real del dispositivo?",
        "## ¿Tienen terceros acceso a la información obtenida por la Aplicación?",
        "## ¿Cuáles son mis derechos de exclusión voluntaria?",
        "## Niños",
        "## Seguridad",
        "## Cambios",
        "## Tu Consentimiento",
        "## Contáctanos",
    ];
    let rus = [
        "## Политика конфиденциальности",
        "## Какую информацию получает Приложение и как она используется?",
        "## Собирает ли Приложение точную информацию о местоположении устройства в реальном времени?",
        "## Видят ли третьи стороны и/или имеют ли доступ к информации, полученной Приложением?",
        "## Каковы мои права на отказ?",
        "## Дети",
        "## Безопасность",
        "## Изменения",
        "## Ваше согласие",
        "## Свяжитесь с нами",
    ];
    for (lang, headings) in [
        (Language::Eng, eng.as_slice()),
        (Language::Spa, spa.as_slice()),
        (Language::Rus, rus.as_slice()),
    ] {
        let text = Msg::PrivacyText.render(lang);
        for heading in headings {
            assert!(
                text.contains(heading),
                "missing sub-heading {heading} for {lang:?}"
            );
        }
    }
}

#[test]
fn license_text_starts_with_mit_title() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let text = Msg::LicenseText.render(lang);
        assert!(
            text.starts_with("The MIT License (MIT)"),
            "license must open with the MIT title for {lang:?}, got: {}",
            text.chars().take(60).collect::<String>(),
        );
    }
}

#[test]
fn widget_chrome_renders_all_languages() {
    let variants = [
        Msg::Cancel,
        Msg::Start,
        Msg::Stop,
        Msg::PickImage,
        Msg::Clear,
        Msg::Preparing,
        Msg::Cancelling,
        Msg::CameraStarting,
        Msg::CameraOff,
        Msg::Decoded,
    ];
    for variant in variants {
        for lang in [Language::Eng, Language::Spa, Language::Rus] {
            assert!(
                !variant.render(lang).is_empty(),
                "empty translation for {variant:?} in {lang:?}"
            );
        }
    }
}
