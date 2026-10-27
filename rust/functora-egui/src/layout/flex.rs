//! Ergonomic flexbox layout builder wrapping `egui_flex::Flex`.

/// A flexbox layout container with CSS-familiar naming.
///
/// Thin wrapper over [`egui_flex::Flex`] that provides shorthand builder
/// methods (`row`, `column`, `gap`, `align_center`, `justify_end`, etc.)
/// so layouts read like CSS flexbox.
///
/// ```no_run
/// # egui::__run_test_ui(|ui| {
/// functora_egui::Flex::row().gap(8.0).show(ui, |f| {
///     f.add(egui::Button::new("Cancel"));
///     f.add(egui::Button::new("Save"));
/// });
/// # });
/// ```
#[must_use]
pub struct Flex {
    pub(crate) inner: egui_flex::Flex,
    wrap: bool,
    single_line_items: bool,
}

impl Flex {
    /// Horizontal (row) layout — items flow left to right. The row wraps onto
    /// multiple lines whenever items do not fit the container, on any
    /// viewport. Use [`Flex::no_wrap`] to keep it on a single line.
    pub fn row() -> Self {
        Self {
            inner: egui_flex::Flex::horizontal(),
            wrap: true,
            single_line_items: false,
        }
    }

    /// Vertical (column) layout — items flow top to bottom.
    pub fn column() -> Self {
        Self {
            inner: egui_flex::Flex::vertical(),
            wrap: false,
            single_line_items: false,
        }
    }

    /// Space between items on both axes (uniform gap).
    pub fn gap(self, gap: f32) -> Self {
        Self {
            inner: self.inner.gap(egui::vec2(gap, gap)),
            ..self
        }
    }

    /// Enable wrapping when items exceed the container width.
    ///
    /// This is already the default for [`Flex::row`].
    pub fn wrap(self) -> Self {
        Self { wrap: true, ..self }
    }

    /// Disables wrapping, keeping the row on a single line even when items
    /// exceed the container width.
    pub fn no_wrap(self) -> Self {
        Self {
            wrap: false,
            ..self
        }
    }

    /// Disables wrapping, keeping the row on a single line even on narrow
    /// screens.
    pub fn no_wrap_on_mobile(self) -> Self {
        self.no_wrap()
    }

    /// Makes items single-line: text truncates with `…` instead of wrapping.
    /// Ideal for toolbars, docks, and status bars where items must share
    /// a consistent height when wrapped to multiple rows.
    pub fn single_line_items(self) -> Self {
        Self {
            single_line_items: true,
            ..self
        }
    }

    /// Cross-axis: align items to start.
    pub fn align_start(self) -> Self {
        Self {
            inner: self.inner.align_items(egui_flex::FlexAlign::Start),
            ..self
        }
    }

    /// Cross-axis: center items.
    pub fn align_center(self) -> Self {
        Self {
            inner: self.inner.align_items(egui_flex::FlexAlign::Center),
            ..self
        }
    }

    /// Cross-axis: align items to end.
    pub fn align_end(self) -> Self {
        Self {
            inner: self.inner.align_items(egui_flex::FlexAlign::End),
            ..self
        }
    }

    /// Cross-axis: stretch items to fill.
    pub fn align_stretch(self) -> Self {
        Self {
            inner: self.inner.align_items(egui_flex::FlexAlign::Stretch),
            ..self
        }
    }

    /// Main-axis: pack items to the start.
    pub fn justify_start(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::Start),
            ..self
        }
    }

    /// Main-axis: center items.
    pub fn justify_center(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::Center),
            ..self
        }
    }

    /// Main-axis: pack items to the end.
    pub fn justify_end(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::End),
            ..self
        }
    }

    /// Main-axis: distribute with equal space between items.
    pub fn justify_between(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::SpaceBetween),
            ..self
        }
    }

    /// Main-axis: distribute with equal space around items.
    pub fn justify_around(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::SpaceAround),
            ..self
        }
    }

    /// Main-axis: distribute with equal space between and around items.
    pub fn justify_evenly(self) -> Self {
        Self {
            inner: self.inner.justify(egui_flex::FlexJustify::SpaceEvenly),
            ..self
        }
    }

    /// Fill all available width.
    pub fn w_full(self) -> Self {
        Self {
            inner: self.inner.w_full(),
            ..self
        }
    }

    /// Fill all available height.
    pub fn h_full(self) -> Self {
        Self {
            inner: self.inner.h_full(),
            ..self
        }
    }

    /// Explicit width in points.
    pub fn width(self, width: f32) -> Self {
        Self {
            inner: self.inner.width(width),
            ..self
        }
    }

    /// Explicit height in points.
    pub fn height(self, height: f32) -> Self {
        Self {
            inner: self.inner.height(height),
            ..self
        }
    }

    /// Default grow factor for all children.
    pub fn grow_items(self, grow: f32) -> Self {
        Self {
            inner: self.inner.grow_items(grow),
            ..self
        }
    }

    /// Display this flex container and populate it via the closure.
    pub fn show<R>(
        self,
        ui: &mut egui::Ui,
        f: impl FnOnce(&mut super::flex_instance::FlexInst) -> R,
    ) -> egui::InnerResponse<R> {
        let flex = if self.wrap {
            self.inner.wrap(true)
        } else {
            self.inner
        };
        ui.scope(|inner_ui| {
            if self.single_line_items {
                inner_ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Truncate);
            }
            flex.show(inner_ui, |instance| {
                let mut inst = super::flex_instance::FlexInst(instance);
                f(&mut inst)
            })
        })
        .inner
    }
}
