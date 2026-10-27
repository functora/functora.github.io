//! shadcn-styled Button builder struct.

/// A button widget styled after shadcn/ui's Button component.
///
/// ```no_run
/// # egui::__run_test_ui(|ui| {
/// if functora_egui::Button::new("Click me").show(ui).clicked() {
///     // handle click
/// }
/// # });
/// ```
#[must_use]
pub struct Button<'a> {
    pub(crate) text: egui::WidgetText,
    pub(crate) variant: crate::tokens::button_variant::ButtonVariant,
    pub(crate) size: crate::tokens::component_size::ComponentSize,
    pub(crate) enabled: bool,
    pub(crate) icon: Option<crate::icons::lucide_icon::LucideIcon>,
    pub(crate) shortcut_text: Option<String>,
    pub(crate) selected: bool,
    pub(crate) full_width: bool,
    pub(crate) fixed_width: Option<f32>,
    pub(crate) wrap: Option<crate::text::Wrap>,
    pub(crate) min_height: f32,
    _phantom: std::marker::PhantomData<&'a ()>,
}

impl Button<'_> {
    pub fn new(text: impl Into<egui::WidgetText>) -> Self {
        Self {
            text: text.into(),
            variant: crate::tokens::button_variant::ButtonVariant::Default,
            size: crate::tokens::component_size::ComponentSize::Default,
            enabled: true,
            icon: None,
            shortcut_text: None,
            selected: false,
            full_width: false,
            fixed_width: None,
            wrap: None,
            min_height: 0.0,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Creates an icon-only button (no text).
    pub fn icon_only(icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        Self {
            text: "".into(),
            variant: crate::tokens::button_variant::ButtonVariant::Default,
            size: crate::tokens::component_size::ComponentSize::Default,
            enabled: true,
            icon: Some(icon),
            shortcut_text: None,
            selected: false,
            full_width: false,
            fixed_width: None,
            wrap: None,
            min_height: 0.0,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn variant(mut self, variant: crate::tokens::button_variant::ButtonVariant) -> Self {
        self.variant = variant;
        self
    }

    pub fn size(mut self, size: crate::tokens::component_size::ComponentSize) -> Self {
        self.size = size;
        self
    }

    pub fn enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    pub fn icon(mut self, icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        self.icon = Some(icon);
        self
    }

    /// Right-aligned muted text (e.g. keyboard shortcut).
    pub fn shortcut_text(mut self, text: impl Into<String>) -> Self {
        self.shortcut_text = Some(text.into());
        self
    }

    /// When true, renders with accent background (for toolbar toggles).
    pub fn selected(mut self, selected: bool) -> Self {
        self.selected = selected;
        self
    }

    /// When true, stretches to fill available width with left-aligned text.
    /// Ideal for menu items and list actions.
    pub fn full_width(mut self) -> Self {
        self.full_width = true;
        self
    }

    /// Forces an exact width (e.g. equal-width tabs in a stretched bar).
    /// Content stays centered; the group ring still wraps the final rect.
    pub fn fixed_width(mut self, width: f32) -> Self {
        self.fixed_width = Some(width);
        self
    }

    /// Overrides how the title breaks when it does not fit the button.
    ///
    /// Defaults to [`crate::text::Wrap::Word`]: the title splits on word
    /// boundaries and the button grows taller when it wraps.
    pub fn wrap(mut self, wrap: crate::text::Wrap) -> Self {
        self.wrap = Some(wrap);
        self
    }

    /// Grows the button to at least `height` (e.g. to keep a wrapping row of
    /// buttons the same height).
    pub fn min_height(mut self, height: f32) -> Self {
        self.min_height = height;
        self
    }

    /// Makes the button single-line: text truncates with `…` instead of wrapping.
    /// Ideal for toolbars, docks, and status bars where buttons must share
    /// a consistent height when wrapped to multiple rows.
    pub fn single_line(mut self) -> Self {
        self.wrap = Some(crate::text::Wrap::Truncate);
        self
    }

    /// Convenience method: adds this widget to the Ui and returns the Response.
    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add_enabled(self.enabled, self)
    }
}
