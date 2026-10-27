//! Temporary context stored in egui data during `ButtonGroup` rendering.

/// Context stored in egui temp data while a `ButtonGroup` is being rendered.
/// Buttons check the `active` flag to adjust their corner radii and borders.
#[derive(Clone, Default)]
pub struct ButtonGroupContext {
    /// Whether a button group is currently being rendered.
    pub active: bool,
    /// Rect of each button in the group, in render order.
    pub rects: Vec<egui::Rect>,
    /// How many buttons were in the group last frame (for first/last detection).
    pub cached_count: usize,
    /// Current button index, incremented by each button.
    pub current_index: usize,
    /// The group's corner radius.
    pub corner_radius: f32,
    /// First/last button index of each row of buttons, from last frame.
    /// Empty until the group has rendered once.
    pub cached_rows: Vec<(usize, usize)>,
    /// Tallest button of the group last frame, so every button of a wrapping
    /// strip can share one row height.
    pub cached_max_height: f32,
}
