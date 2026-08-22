//! Viewport breakpoints for adaptive layouts.

/// Width-based viewport breakpoint, mirroring `functora-css`'s
/// `@mobile: 50rem` (800px) boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Breakpoint {
    /// Narrow viewport (under 800px): phones and small tablets in portrait.
    Mobile,
    /// Wide viewport (800px and up): tablets in landscape and desktops.
    #[default]
    Desktop,
}

impl Breakpoint {
    /// Viewport width below which the `Mobile` scale applies (`50rem` at
    /// 16px root font size, matching `functora-css`'s `@mobile` variable).
    pub const MOBILE_MAX_WIDTH: f32 = 800.0;

    /// Classifies a viewport width into a breakpoint.
    pub fn from_width(width: f32) -> Self {
        if width < Self::MOBILE_MAX_WIDTH {
            Self::Mobile
        } else {
            Self::Desktop
        }
    }

    /// True on narrow viewports where touch-friendly sizing is used.
    pub fn is_mobile(self) -> bool {
        self == Self::Mobile
    }

    /// True on wide viewports where mouse-friendly compact sizing is used.
    pub fn is_desktop(self) -> bool {
        self == Self::Desktop
    }
}
