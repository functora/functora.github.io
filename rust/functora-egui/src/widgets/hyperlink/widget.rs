//! Hyperlink builder — single inline link styled with shadcn theme.

/// A single hyperlink: primary-colored text with underline on hover.
#[must_use]
pub struct Hyperlink {
    pub(crate) label: String,
    pub(crate) url: Option<String>,
    pub(crate) new_tab: bool,
}

impl Hyperlink {
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            url: None,
            new_tab: true,
        }
    }

    pub fn url(mut self, url: impl Into<String>) -> Self {
        self.url = Some(url.into());
        self
    }

    pub fn open_in_new_tab(mut self, new_tab: bool) -> Self {
        self.new_tab = new_tab;
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
