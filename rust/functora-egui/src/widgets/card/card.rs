//! Card builder struct — a bordered container styled after shadcn/ui Card.

/// A card container: `ring-foreground/10 bg-card rounded-xl py-4 ring-1`.
///
/// Not a `Widget` — uses `show(ui, closure)` pattern like `egui::Frame`.
#[must_use]
pub struct Card {
    pub(crate) heading: Option<String>,
}

impl Card {
    pub fn new() -> Self {
        Self { heading: None }
    }

    /// Adds a strong heading rendered above the card content.
    pub fn heading(mut self, heading: impl Into<String>) -> Self {
        self.heading = Some(heading.into());
        self
    }
}
