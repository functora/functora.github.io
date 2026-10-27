//! Label builder struct — styled text following shadcn/ui's `text-sm font-medium`.

/// A styled text label: `text-sm font-medium`.
#[must_use]
pub struct Label {
    pub(crate) text: String,
    pub(crate) muted: bool,
    pub(crate) size: Option<crate::tokens::component_size::ComponentSize>,
    pub(crate) wrap: Option<crate::text::Wrap>,
}

impl Label {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            muted: false,
            size: None,
            wrap: None,
        }
    }

    /// Use muted foreground color.
    pub fn muted(mut self) -> Self {
        self.muted = true;
        self
    }

    /// Match a component size for consistent baseline alignment with buttons.
    /// Sets both the font size and allocated height to match the given size.
    pub fn size(mut self, size: crate::tokens::component_size::ComponentSize) -> Self {
        self.size = Some(size);
        self
    }

    /// Overrides how the text breaks when it does not fit the available width.
    ///
    /// Defaults to [`crate::text::Wrap::Word`]: long text wraps onto more
    /// lines and the label grows taller instead of wider.
    pub fn wrap(mut self, wrap: crate::text::Wrap) -> Self {
        self.wrap = Some(wrap);
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
