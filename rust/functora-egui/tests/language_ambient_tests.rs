//! Widget strings follow the ambient language: the app stores one language
//! and every widget that renders its own labels (select placeholder,
//! combobox search, calendar month and weekday names) renders in it,
//! falling back to English until the app stores one.

pub mod common;
use common::{Harness, all_texts};

use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Calendar, SelectLabeled, set_language};

#[test]
fn paste_and_copy_messages_report_different_outcomes() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let pasted = BaseMsg::Pasted.render(lang);
        let copied = BaseMsg::Copied.render(lang);
        assert!(!pasted.is_empty(), "Pasted must carry text for {lang:?}");
        assert!(!copied.is_empty(), "Copied must carry text for {lang:?}");
        assert_ne!(
            pasted, copied,
            "a paste toast must not read like a copy toast in {lang:?}"
        );
    }
}

#[test]
fn select_placeholder_follows_the_ambient_language() {
    let entries = [(0_u8, "Alpha".to_owned()), (1, "Beta".to_owned())];
    let mut selected: Option<u8> = None;
    let mut app = Harness::new(egui::Vec2::new(400.0, 200.0));

    let default_out = app.step(&mut |ui| {
        let _ = SelectLabeled::new(&mut selected, &entries).show(ui);
    });
    assert!(
        all_texts(&default_out)
            .iter()
            .any(|(_, text)| text == "Select..."),
        "without a language the placeholder must default to English"
    );

    set_language(&app.ctx, Language::Rus);
    let rus_out = app.step(&mut |ui| {
        let _ = SelectLabeled::new(&mut selected, &entries).show(ui);
    });
    assert!(
        all_texts(&rus_out)
            .iter()
            .any(|(_, text)| text == "Выбрать..."),
        "the placeholder must follow the stored language"
    );

    set_language(&app.ctx, Language::Spa);
    let spa_out = app.step(&mut |ui| {
        let _ = SelectLabeled::new(&mut selected, &entries).show(ui);
    });
    assert!(
        all_texts(&spa_out)
            .iter()
            .any(|(_, text)| text == "Seleccionar..."),
        "the placeholder must follow the stored language"
    );
}

#[test]
fn calendar_names_follow_the_ambient_language() {
    let mut day = 1_u32;
    let mut month = 1_u32;
    let mut year = 2026_i32;
    let mut app = Harness::new(egui::Vec2::new(400.0, 400.0));

    let default_out = app.step(&mut |ui| {
        let _ = Calendar::new().show(ui, &mut year, &mut month, &mut day);
    });
    let default_texts = all_texts(&default_out);
    assert!(
        default_texts.iter().any(|(_, text)| text == "Mo"),
        "weekday headers must default to English, got {default_texts:?}"
    );
    assert!(
        default_texts
            .iter()
            .any(|(_, text)| text.contains("January")),
        "the month must default to English, got {default_texts:?}"
    );

    set_language(&app.ctx, Language::Rus);
    let rus_out = app.step(&mut |ui| {
        let _ = Calendar::new().show(ui, &mut year, &mut month, &mut day);
    });
    let rus_texts = all_texts(&rus_out);
    assert!(
        rus_texts.iter().any(|(_, text)| text == "Пн"),
        "weekday headers must follow the stored language, got {rus_texts:?}"
    );
    assert!(
        rus_texts.iter().any(|(_, text)| text.contains("Январь")),
        "the month must follow the stored language, got {rus_texts:?}"
    );
}

#[test]
fn month_and_weekday_tables_cover_every_value() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let months = (1..=12)
            .map(|month| functora_egui::language::month_name(month, lang))
            .collect::<Vec<_>>();
        assert!(
            months
                .iter()
                .all(|name| !name.is_empty() && *name != "Unknown"),
            "every month must have a name in {lang:?}, got {months:?}"
        );
        let weekdays = (0..7)
            .map(|day| functora_egui::language::weekday_short(day, lang))
            .collect::<Vec<_>>();
        assert!(
            weekdays.iter().all(|name| !name.is_empty()),
            "every weekday must have a name in {lang:?}, got {weekdays:?}"
        );
    }
    assert_eq!(
        functora_egui::language::month_name(13, Language::Eng),
        "Unknown",
        "an out-of-range month must fall back to Unknown"
    );
}
