//! `NavigationMenu` builder struct — a horizontal navigation bar.

/// A navigation menu: horizontal bar of clickable links.
#[must_use]
pub struct NavigationMenu {
    pub(crate) items: Vec<String>,
}

impl NavigationMenu {
    pub fn new(items: Vec<String>) -> Self {
        Self { items }
    }
}

/// A navigation menu bound to enum values instead of blind indexes:
/// each entry pairs a value with its label.
#[must_use]
pub struct NavigationMenuValue<'a, T: Clone + PartialEq> {
    pub(crate) entries: &'a [(T, String)],
}

impl<'a, T: Clone + PartialEq> NavigationMenuValue<'a, T> {
    pub fn new(entries: &'a [(T, String)]) -> Self {
        Self { entries }
    }
}
