//! Hypertext builder — paragraph with inline links.

/// A segment inside hypertext.
#[derive(Debug, Clone)]
pub enum Segment {
    Text(String),
    Link {
        label: String,
        url: String,
    },
    /// Renders exactly like a link but reports its `id` on click instead of
    /// opening a URL. For internal navigation (footers, legal text).
    Action {
        label: String,
        id: String,
    },
}

#[must_use]
pub struct Hypertext {
    pub(crate) segments: Vec<Segment>,
    pub(crate) centered: bool,
    pub(crate) size: f32,
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
            size: 12.0,
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

    pub fn action(mut self, label: impl Into<String>, id: impl Into<String>) -> Self {
        self.segments.push(Segment::Action {
            label: label.into(),
            id: id.into(),
        });
        self
    }

    pub fn centered(mut self) -> Self {
        self.centered = true;
        self
    }

    /// Base font size for every segment. Text and links always share one
    /// size so mixed paragraphs stay uniform.
    pub fn size(mut self, size: f32) -> Self {
        self.size = size;
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        self.show_action(ui).0
    }

    /// Shows the paragraph and reports the clicked action id, if an action
    /// segment was clicked. Link segments keep opening their URL.
    pub fn show_action(self, ui: &mut egui::Ui) -> (egui::Response, Option<String>) {
        self.show_inner(ui)
    }
}
