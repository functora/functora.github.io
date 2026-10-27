//! Table cells: a cell whose content is one long unbroken word must stay
//! inside its column, breaking between characters as a last resort, instead
//! of painting the word across the neighboring columns.

pub mod common;
use common::{Harness, all_texts};

use egui::{Rect, Vec2};

const SCREEN: Vec2 = Vec2::new(320.0, 640.0);
const LONG_WORD: &str = "https://functora.github.io/apps/demo/?page=about&lang=en&theme=dark";

fn long_word_rects(texts: &[(Rect, String)]) -> Vec<(Rect, String)> {
    texts
        .iter()
        .filter(|(_, text)| text.contains(LONG_WORD))
        .cloned()
        .collect()
}

fn table_texts(body: &mut dyn FnMut(&mut egui::Ui)) -> Vec<(Rect, String)> {
    all_texts(&Harness::new(Vec2::new(320.0, 640.0)).step(body))
}

#[test]
fn long_cell_word_breaks_inside_its_column() {
    let mut texts = table_texts(&mut |ui| {
        let _ = functora_egui::Table::new(vec!["Name".to_owned(), "Link".to_owned()])
            .rows(vec![vec!["Demo".to_owned(), LONG_WORD.to_owned()]])
            .show(ui);
    });

    let cells = long_word_rects(&texts);
    assert!(
        !cells.is_empty(),
        "the long word must render, got {texts:?}"
    );
    let column = SCREEN.x / 2.0;
    for (rect, text) in &cells {
        assert!(
            rect.width() <= column,
            "cell {text:?} at {rect:?} is wider than its {column}px column"
        );
        assert!(
            rect.max.x <= SCREEN.x,
            "cell {text:?} at {rect:?} escapes the viewport"
        );
    }
    texts.retain(|(_, text)| text.contains("Demo"));
    let (rect, _) = texts
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("the other cell must render"));
    for (word_rect, _) in &cells {
        assert!(
            word_rect.min.x >= rect.max.x || word_rect.max.x <= rect.min.x,
            "long word at {word_rect:?} overlaps the neighboring cell {rect:?}"
        );
    }
}

#[test]
fn wrapped_cell_grows_its_row_instead_of_its_column() {
    let cells = table_texts(&mut |ui| {
        let _ = functora_egui::Table::new(vec!["Name".to_owned(), "Link".to_owned()])
            .rows(vec![vec![
                "Demo".to_owned(),
                format!("{LONG_WORD} {LONG_WORD}"),
            ]])
            .show(ui);
    })
    .into_iter()
    .filter(|(_, text)| text.contains("functora"))
    .collect::<Vec<_>>();
    assert!(!cells.is_empty(), "the wrapped cell must render");
    for (rect, _) in &cells {
        assert!(
            rect.height() > 20.0,
            "a cell holding two long words must wrap onto several rows at {rect:?}"
        );
    }
}
