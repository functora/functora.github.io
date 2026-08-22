//! Extension trait for querying the current viewport breakpoint.

use super::breakpoint::Breakpoint;
use super::spacing::Spacing;

/// Adds mobile-first breakpoint queries to egui contexts and UIs.
pub trait ResponsiveExt {
    /// The viewport width-based breakpoint of the current frame.
    fn breakpoint(&self) -> Breakpoint;

    /// True when the viewport is narrower than the mobile breakpoint.
    fn on_mobile(&self) -> bool;

    /// True when the viewport is at least the mobile breakpoint width.
    fn on_desktop(&self) -> bool;

    /// The spacing scale matching the current viewport.
    fn responsive_spacing(&self) -> Spacing;
}

impl ResponsiveExt for egui::Context {
    fn breakpoint(&self) -> Breakpoint {
        Breakpoint::from_width(self.input(|i| i.viewport_rect().width()))
    }

    fn on_mobile(&self) -> bool {
        self.breakpoint().is_mobile()
    }

    fn on_desktop(&self) -> bool {
        self.breakpoint().is_desktop()
    }

    fn responsive_spacing(&self) -> Spacing {
        Spacing::for_breakpoint(self.breakpoint())
    }
}

impl ResponsiveExt for egui::Ui {
    fn breakpoint(&self) -> Breakpoint {
        self.ctx().breakpoint()
    }

    fn on_mobile(&self) -> bool {
        self.ctx().on_mobile()
    }

    fn on_desktop(&self) -> bool {
        self.ctx().on_desktop()
    }

    fn responsive_spacing(&self) -> Spacing {
        self.ctx().responsive_spacing()
    }
}
