//! Separator builder struct — a 1px line divider.

/// A horizontal or vertical separator line.
#[must_use]
pub struct Separator {
    pub(crate) horizontal: bool,
    pub(crate) text: Option<String>,
    pub(crate) icon: Option<crate::icons::lucide_icon::LucideIcon>,
}

impl Separator {
    pub fn horizontal() -> Self {
        Self {
            horizontal: true,
            text: None,
            icon: None,
        }
    }

    pub fn vertical() -> Self {
        Self {
            horizontal: false,
            text: None,
            icon: None,
        }
    }

    /// Adds a centered text label to the separator.
    pub fn text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    pub fn icon(mut self, icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        self.icon = Some(icon);
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
