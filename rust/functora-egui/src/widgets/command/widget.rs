//! Command builder struct — a command palette with search.

use crate::icons::lucide_icon::LucideIcon;

/// A single palette entry with group and item icons.
#[derive(Clone, Debug)]
pub struct CommandItem {
    pub group: String,
    pub group_icon: LucideIcon,
    pub label: String,
    pub icon: LucideIcon,
}

impl From<(String, String)> for CommandItem {
    fn from((group, label): (String, String)) -> Self {
        Self {
            group,
            group_icon: LucideIcon::Component,
            label,
            icon: LucideIcon::Component,
        }
    }
}

/// A command palette: centered modal with search input and command list.
#[must_use]
pub struct Command {
    pub(crate) items: Vec<CommandItem>,
    pub(crate) placeholder: String,
}

impl Command {
    /// Items are `(group_name, command_label)` pairs.
    pub fn new(items: Vec<(String, String)>) -> Self {
        Self {
            items: items.into_iter().map(CommandItem::from).collect(),
            placeholder: "Type a command or search...".to_owned(),
        }
    }

    /// Items with icons – the preferred constructor when icons are available.
    pub fn with_items(items: Vec<CommandItem>) -> Self {
        Self {
            items,
            placeholder: "Type a command or search...".to_owned(),
        }
    }

    pub fn placeholder(mut self, text: impl Into<String>) -> Self {
        self.placeholder = text.into();
        self
    }
}
