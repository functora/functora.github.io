//! Combobox builder struct — a searchable dropdown select.

/// A combobox: input with dropdown filter list.
#[must_use]
pub struct Combobox {
    pub(crate) items: Vec<String>,
    pub(crate) placeholder: String,
    pub(crate) width: Option<f32>,
}

impl Combobox {
    pub fn new(items: Vec<String>) -> Self {
        Self {
            items,
            placeholder: "Select...".to_owned(),
            width: None,
        }
    }

    pub fn placeholder(mut self, text: impl Into<String>) -> Self {
        self.placeholder = text.into();
        self
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = Some(width);
        self
    }
}

/// A combobox bound to enum values instead of blind indexes:
/// each entry pairs a value with its label.
#[must_use]
pub struct ComboboxValue<'a, T: Clone + PartialEq> {
    pub(crate) entries: &'a [(T, String)],
    pub(crate) placeholder: String,
    pub(crate) width: Option<f32>,
}

impl<'a, T: Clone + PartialEq> ComboboxValue<'a, T> {
    pub fn new(entries: &'a [(T, String)]) -> Self {
        Self {
            entries,
            placeholder: "Select...".to_owned(),
            width: None,
        }
    }

    pub fn placeholder(mut self, text: impl Into<String>) -> Self {
        self.placeholder = text.into();
        self
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = Some(width);
        self
    }
}
