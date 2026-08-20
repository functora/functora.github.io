//! Ergonomic flexbox layout builder wrapping `egui_flex::Flex`.

use crate::responsive::responsive_ext::ResponsiveExt;

/// A flexbox layout container with CSS-familiar naming.
///
/// Thin wrapper over [`egui_flex::Flex`] that provides shorthand builder
/// methods (`row`, `column`, `gap`, `align_center`, `justify_end`, etc.)
/// so layouts read like CSS flexbox.
///
/// ```no_run
/// # egui::__run_test_ui(|ui| {
/// egui_shadcn::Flex::row().gap(8.0).show(ui, |f| {
///     f.add(egui::Button::new("Cancel"));
///     f.add(egui::Button::new("Save"));
/// });
/// # });
/// ```
#[must_use]
pub struct Flex {
    pub(crate) flex: egui_flex::Flex,
    wrap_on_mobile: bool,
}

impl Flex {
    /// Horizontal (row) layout — items flow left to right. On mobile
    /// viewports the row wraps onto multiple lines when items overflow.
    pub fn row() -> Self {
        Self {
            flex: egui_flex::Flex::horizontal(),
            wrap_on_mobile: true,
        }
    }

    /// Vertical (column) layout — items flow top to bottom.
    pub fn column() -> Self {
        Self {
            flex: egui_flex::Flex::vertical(),
            wrap_on_mobile: false,
        }
    }

    /// Space between items on both axes (uniform gap).
    pub fn gap(self, gap: f32) -> Self {
        Self {
            flex: self.flex.gap(egui::vec2(gap, gap)),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Enable wrapping when items exceed the container width.
    pub fn wrap(self) -> Self {
        Self {
            flex: self.flex.wrap(true),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Disables mobile wrapping, keeping the row on a single line even on
    /// narrow screens.
    pub fn no_wrap_on_mobile(self) -> Self {
        Self {
            flex: self.flex,
            wrap_on_mobile: false,
        }
    }

    /// Cross-axis: align items to start.
    pub fn align_start(self) -> Self {
        Self {
            flex: self.flex.align_items(egui_flex::FlexAlign::Start),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Cross-axis: center items.
    pub fn align_center(self) -> Self {
        Self {
            flex: self.flex.align_items(egui_flex::FlexAlign::Center),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Cross-axis: align items to end.
    pub fn align_end(self) -> Self {
        Self {
            flex: self.flex.align_items(egui_flex::FlexAlign::End),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Cross-axis: stretch items to fill.
    pub fn align_stretch(self) -> Self {
        Self {
            flex: self.flex.align_items(egui_flex::FlexAlign::Stretch),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: pack items to the start.
    pub fn justify_start(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::Start),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: center items.
    pub fn justify_center(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::Center),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: pack items to the end.
    pub fn justify_end(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::End),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: distribute with equal space between items.
    pub fn justify_between(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::SpaceBetween),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: distribute with equal space around items.
    pub fn justify_around(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::SpaceAround),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Main-axis: distribute with equal space between and around items.
    pub fn justify_evenly(self) -> Self {
        Self {
            flex: self.flex.justify(egui_flex::FlexJustify::SpaceEvenly),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Fill all available width.
    pub fn w_full(self) -> Self {
        Self {
            flex: self.flex.w_full(),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Fill all available height.
    pub fn h_full(self) -> Self {
        Self {
            flex: self.flex.h_full(),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Explicit width in points.
    pub fn width(self, width: f32) -> Self {
        Self {
            flex: self.flex.width(width),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Explicit height in points.
    pub fn height(self, height: f32) -> Self {
        Self {
            flex: self.flex.height(height),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Default grow factor for all children.
    pub fn grow_items(self, grow: f32) -> Self {
        Self {
            flex: self.flex.grow_items(grow),
            wrap_on_mobile: self.wrap_on_mobile,
        }
    }

    /// Display this flex container and populate it via the closure.
    pub fn show<R>(
        self,
        ui: &mut egui::Ui,
        f: impl FnOnce(&mut super::flex_instance::FlexInst) -> R,
    ) -> egui::InnerResponse<R> {
        let flex = if self.wrap_on_mobile && ui.on_mobile() {
            self.flex.wrap(true)
        } else {
            self.flex
        };
        flex.show(ui, |instance| {
            let mut inst = super::flex_instance::FlexInst(instance);
            f(&mut inst)
        })
    }
}
