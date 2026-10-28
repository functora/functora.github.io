//! Ambient UI language: the app stores the active language once per frame
//! and every widget that renders its own labels follows it.
//!
//! [`Shell`] propagates the language it is configured with, so apps using
//! the shell need no extra wiring.
//!
//! [`Shell`]: crate::Shell

use crate::i18n::{I18N, Language};
use crate::messages::Msg;

fn language_id() -> egui::Id {
    egui::Id::unique("functora_language")
}

/// Stores the active UI language for every widget to render in.
pub fn set_language(ctx: &egui::Context, lang: Language) {
    let _ = ctx.data_mut(|d| d.insert_temp(language_id(), lang));
}

/// The active UI language; English until the app stores one.
#[must_use]
pub fn current_language(ctx: &egui::Context) -> Language {
    ctx.data(|d| d.get_temp::<Language>(language_id()))
        .unwrap_or(Language::Eng)
}

/// Widget label in the active UI language.
pub(crate) fn label(msg: &Msg, ctx: &egui::Context) -> String {
    msg.render(current_language(ctx))
}

/// Localized month name, 1 = January.
#[must_use]
pub fn month_name(month: u32, lang: Language) -> &'static str {
    match (month, lang) {
        (1, Language::Eng) => "January",
        (2, Language::Eng) => "February",
        (3, Language::Eng) => "March",
        (4, Language::Eng) => "April",
        (5, Language::Eng) => "May",
        (6, Language::Eng) => "June",
        (7, Language::Eng) => "July",
        (8, Language::Eng) => "August",
        (9, Language::Eng) => "September",
        (10, Language::Eng) => "October",
        (11, Language::Eng) => "November",
        (12, Language::Eng) => "December",
        (1, Language::Spa) => "Enero",
        (2, Language::Spa) => "Febrero",
        (3, Language::Spa) => "Marzo",
        (4, Language::Spa) => "Abril",
        (5, Language::Spa) => "Mayo",
        (6, Language::Spa) => "Junio",
        (7, Language::Spa) => "Julio",
        (8, Language::Spa) => "Agosto",
        (9, Language::Spa) => "Septiembre",
        (10, Language::Spa) => "Octubre",
        (11, Language::Spa) => "Noviembre",
        (12, Language::Spa) => "Diciembre",
        (1, Language::Rus) => "Январь",
        (2, Language::Rus) => "Февраль",
        (3, Language::Rus) => "Март",
        (4, Language::Rus) => "Апрель",
        (5, Language::Rus) => "Май",
        (6, Language::Rus) => "Июнь",
        (7, Language::Rus) => "Июль",
        (8, Language::Rus) => "Август",
        (9, Language::Rus) => "Сентябрь",
        (10, Language::Rus) => "Октябрь",
        (11, Language::Rus) => "Ноябрь",
        (12, Language::Rus) => "Декабрь",
        _ => "Unknown",
    }
}

/// Localized weekday abbreviation, 0 = Monday.
#[must_use]
pub fn weekday_short(weekday: u32, lang: Language) -> &'static str {
    match (weekday, lang) {
        (0, Language::Eng) => "Mo",
        (1, Language::Eng) => "Tu",
        (2, Language::Eng) => "We",
        (3, Language::Eng) => "Th",
        (4, Language::Eng) => "Fr",
        (5, Language::Eng) => "Sa",
        (6, Language::Eng) => "Su",
        (0, Language::Spa) => "Lu",
        (1, Language::Spa) => "Ma",
        (2, Language::Spa) => "Mi",
        (3, Language::Spa) => "Ju",
        (4, Language::Spa) => "Vi",
        (5, Language::Spa) => "Sá",
        (6, Language::Spa) => "Do",
        (0, Language::Rus) => "Пн",
        (1, Language::Rus) => "Вт",
        (2, Language::Rus) => "Ср",
        (3, Language::Rus) => "Чт",
        (4, Language::Rus) => "Пт",
        (5, Language::Rus) => "Сб",
        (6, Language::Rus) => "Вс",
        _ => "",
    }
}
