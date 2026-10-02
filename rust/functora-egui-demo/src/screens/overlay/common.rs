#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContextAction {
    Cut,
    Copy,
    Paste,
    SelectAll,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProfileAction {
    Profile,
    Settings,
    LogOut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EditAction {
    Undo,
    Redo,
    Cut,
    Copy,
    Paste,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ViewAction {
    ZoomIn,
    ZoomOut,
    FullScreen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HelpAction {
    Documentation,
    About,
}
