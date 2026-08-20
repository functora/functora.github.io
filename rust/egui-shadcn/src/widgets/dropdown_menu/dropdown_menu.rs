//! DropdownMenu builder — popup menu with styled items.

/// A dropdown menu popup with shadcn styling.
pub struct DropdownMenu;

/// A rich menu item with optional shortcut, enabled state, and separator support.
pub enum MenuItem {
    /// A clickable item with label, optional shortcut, optional selected check, and optional enabled state.
    Item {
        label: String,
        shortcut: Option<String>,
        selected: bool,
        enabled: bool,
    },
    /// A horizontal separator line.
    Separator,
}

impl MenuItem {
    /// Simple item with just a label.
    pub fn label(label: impl Into<String>) -> Self {
        Self::Item {
            label: label.into(),
            shortcut: None,
            selected: false,
            enabled: true,
        }
    }

    /// Item with label and keyboard shortcut text.
    pub fn with_shortcut(label: impl Into<String>, shortcut: impl Into<String>) -> Self {
        Self::Item {
            label: label.into(),
            shortcut: Some(shortcut.into()),
            selected: false,
            enabled: true,
        }
    }

    /// Marks the item with a leading check mark.
    pub fn selected(mut self, selected: bool) -> Self {
        if let Self::Item { selected: sel, .. } = &mut self {
            *sel = selected;
        }
        self
    }

    /// Item with label, shortcut, and enabled state.
    pub fn full(label: impl Into<String>, shortcut: Option<String>, enabled: bool) -> Self {
        Self::Item {
            label: label.into(),
            shortcut,
            selected: false,
            enabled,
        }
    }

    /// A separator line.
    pub fn separator() -> Self {
        Self::Separator
    }
}
