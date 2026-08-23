//! Resizable builder struct — a horizontal split panel.

/// A resizable split panel with a draggable handle.
#[must_use]
pub struct Resizable {
    pub(crate) height: f32,
}

impl Default for Resizable {
    fn default() -> Self {
        Self::new()
    }
}

impl Resizable {
    pub fn new() -> Self {
        Self { height: 200.0 }
    }

    /// Sets the panel height in points.
    pub fn height(mut self, height: f32) -> Self {
        self.height = height;
        self
    }
}
