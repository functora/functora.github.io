//! Sidebar builder struct -- app sidebar navigation.

/// A sidebar: `w-64 border-l bg-sidebar h-full`.
#[must_use]
pub struct Sidebar {
    pub(crate) width: f32,
    pub(crate) collapsible: bool,
    pub(crate) responsive: bool,
}

impl Sidebar {
    pub fn new() -> Self {
        Self {
            width: 256.0,
            collapsible: false,
            responsive: true,
        }
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = width;
        self
    }

    pub fn collapsible(mut self) -> Self {
        self.collapsible = true;
        self
    }

    /// Keeps the sidebar as a static inline panel on all viewports, so it
    /// never collapses into the mobile overlay drawer.
    pub fn static_(mut self) -> Self {
        self.responsive = false;
        self
    }
}
