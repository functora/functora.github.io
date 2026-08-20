//! ToggleGroup builder struct — a set of exclusive toggle buttons.

/// A group of toggle buttons: `inline-flex gap-0.5 rounded-lg bg-muted p-0.5`.
#[must_use]
pub struct ToggleGroup {
    pub(crate) items: Vec<String>,
    pub(crate) icons: Vec<Option<crate::icons::lucide_icon::LucideIcon>>,
    pub(crate) variant: crate::tokens::toggle_variant::ToggleVariant,
}

impl ToggleGroup {
    pub fn new(items: Vec<String>) -> Self {
        let len = items.len();
        Self {
            items,
            icons: vec![None; len],
            variant: crate::tokens::toggle_variant::ToggleVariant::Default,
        }
    }

    /// Attaches one icon per item. Fewer icons than items leaves the rest icon-less;
    /// extra icons are ignored.
    pub fn icons(mut self, icons: Vec<crate::icons::lucide_icon::LucideIcon>) -> Self {
        self.icons = icons.into_iter().map(Some).collect();
        self.icons.resize(self.items.len(), None);
        self
    }

    pub fn variant(mut self, variant: crate::tokens::toggle_variant::ToggleVariant) -> Self {
        self.variant = variant;
        self
    }
}
