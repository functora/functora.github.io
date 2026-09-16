//! Tabs builder struct — a tabbed content switcher.

/// A tabbed container: `bg-muted rounded-lg p-0.5` tab bar with content area below.
#[must_use]
pub struct Tabs {
    pub(crate) labels: Vec<String>,
    pub(crate) fill_width: bool,
}

impl Tabs {
    pub fn new(labels: Vec<String>) -> Self {
        Self {
            labels,
            fill_width: false,
        }
    }

    /// Stretch the tab bar to the available width with equal-width tabs.
    pub fn fill_width(mut self) -> Self {
        self.fill_width = true;
        self
    }
}

/// A tabbed container bound to enum values instead of blind indexes:
/// each entry pairs a value with its label.
#[must_use]
pub struct TabsValue<'a, T: Clone + PartialEq> {
    pub(crate) entries: &'a [(T, String)],
    pub(crate) fill_width: bool,
}

impl<'a, T: Clone + PartialEq> TabsValue<'a, T> {
    pub fn new(entries: &'a [(T, String)]) -> Self {
        Self {
            entries,
            fill_width: false,
        }
    }

    /// Stretch the tab bar to the available width with equal-width tabs.
    pub fn fill_width(mut self) -> Self {
        self.fill_width = true;
        self
    }
}

/// Tab entry that can be a text label or icon with tooltip.
pub enum TabEntry {
    Text(String),
    Icon {
        icon: crate::icons::lucide_icon::LucideIcon,
        tooltip: String,
    },
}

/// Icon-based tabs variant.
#[must_use]
pub struct IconTabs {
    pub(crate) entries: Vec<TabEntry>,
    pub(crate) fill_width: bool,
}

impl IconTabs {
    pub fn new(entries: Vec<TabEntry>) -> Self {
        Self {
            entries,
            fill_width: false,
        }
    }

    /// Stretch the tab bar to the available width with equal-width tabs.
    pub fn fill_width(mut self) -> Self {
        self.fill_width = true;
        self
    }
}

/// Icon-based tabs bound to enum values instead of blind indexes:
/// each entry pairs a value with its tab entry.
#[must_use]
pub struct IconTabsValue<'a, T: Clone + PartialEq> {
    pub(crate) entries: &'a [(T, TabEntry)],
    pub(crate) fill_width: bool,
}

impl<'a, T: Clone + PartialEq> IconTabsValue<'a, T> {
    pub fn new(entries: &'a [(T, TabEntry)]) -> Self {
        Self {
            entries,
            fill_width: false,
        }
    }

    /// Stretch the tab bar to the available width with equal-width tabs.
    pub fn fill_width(mut self) -> Self {
        self.fill_width = true;
        self
    }
}
