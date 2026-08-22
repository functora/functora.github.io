//! Responsive spacing scale shared by all widgets.

use super::breakpoint::Breakpoint;

/// Per-breakpoint spacing and sizing values.
///
/// Values mirror `functora-css`'s variables (`@width: 90rem` for the content
/// column, `2rem` page padding) plus touch-friendly control sizing on mobile.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spacing {
    /// The breakpoint this scale belongs to.
    pub breakpoint: Breakpoint,
    /// Minimum height of interactive controls (buttons, inputs, selects).
    /// Mobile uses the 44pt touch-target guideline.
    pub touch_height: f32,
    /// Horizontal padding inside interactive controls.
    pub touch_padding: f32,
    /// Gap between sibling widgets.
    pub gap: f32,
    /// Padding around page content.
    pub page_padding: f32,
    /// Maximum width of the content column (`90rem`).
    pub content_max_width: f32,
}

impl Spacing {
    /// Compact scale for mouse-driven desktop use.
    pub fn desktop() -> Self {
        Self {
            breakpoint: Breakpoint::Desktop,
            touch_height: 32.0,
            touch_padding: 10.0,
            gap: 8.0,
            page_padding: 32.0,
            content_max_width: 1440.0,
        }
    }

    /// Touch-friendly scale for narrow viewports.
    pub fn mobile() -> Self {
        Self {
            breakpoint: Breakpoint::Mobile,
            touch_height: 44.0,
            touch_padding: 14.0,
            gap: 12.0,
            page_padding: 16.0,
            content_max_width: 1440.0,
        }
    }

    /// The scale matching the given breakpoint.
    pub fn for_breakpoint(breakpoint: Breakpoint) -> Self {
        match breakpoint {
            Breakpoint::Mobile => Self::mobile(),
            Breakpoint::Desktop => Self::desktop(),
        }
    }

    /// True when this scale is the touch-friendly mobile one.
    pub fn is_mobile(self) -> bool {
        self.breakpoint.is_mobile()
    }
}
