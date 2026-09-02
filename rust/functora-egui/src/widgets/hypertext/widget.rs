//! Hypertext builder — paragraph with inline links.

/// A segment inside hypertext.
#[derive(Debug, Clone)]
pub enum Segment {
    Text(String),
    Link { label: String, url: String },
}

#[must_use]
pub struct Hypertext {
    pub(crate) segments: Vec<Segment>,
    pub(crate) centered: bool,
}

impl Default for Hypertext {
    fn default() -> Self {
        Self::new()
    }
}

impl Hypertext {
    pub fn new() -> Self {
        Self {
            segments: Vec::new(),
            centered: false,
        }
    }

    pub fn text(mut self, text: impl Into<String>) -> Self {
        let txt = text.into();
        if !txt.is_empty() {
            self.segments.push(Segment::Text(txt));
        }
        self
    }

    pub fn link(mut self, label: impl Into<String>, url: impl Into<String>) -> Self {
        self.segments.push(Segment::Link {
            label: label.into(),
            url: url.into(),
        });
        self
    }

    pub fn centered(mut self) -> Self {
        self.centered = true;
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
