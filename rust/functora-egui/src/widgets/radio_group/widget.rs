//! `RadioGroup` builder struct — proper grouped radio buttons with mutual exclusion.

/// A group of radio buttons managing mutual exclusion internally.
#[must_use]
pub struct RadioGroup<'a, T: Clone + PartialEq + std::fmt::Display> {
    pub(crate) selected: &'a mut T,
    pub(crate) options: &'a [T],
}

impl<'a, T: Clone + PartialEq + std::fmt::Display> RadioGroup<'a, T> {
    pub fn new(selected: &'a mut T, options: &'a [T]) -> Self {
        Self { selected, options }
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}

/// A radio group bound to enum values with explicit labels instead of
/// `Display`: each entry pairs a value with its (possibly localized) label.
#[must_use]
pub struct RadioGroupLabeled<'a, T: Clone + PartialEq> {
    pub(crate) selected: &'a mut T,
    pub(crate) entries: &'a [(T, String)],
}

impl<'a, T: Clone + PartialEq> RadioGroupLabeled<'a, T> {
    pub fn new(selected: &'a mut T, entries: &'a [(T, String)]) -> Self {
        Self { selected, entries }
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
