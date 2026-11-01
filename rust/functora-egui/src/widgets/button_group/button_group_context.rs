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
    /// Tallest height any button of the group wanted last frame — the button's
    /// own height before group-wide sharing is applied, so sharing never keeps
    /// a height the current layout no longer needs.
    pub cached_max_height: f32,
    /// Tallest height any button of the group wanted this frame, collected
    /// while the buttons render and written back as the next frame's
    /// [`Self::cached_max_height`].
    pub wanted_max_height: f32,
}
