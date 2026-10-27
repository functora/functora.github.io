//! Interaction flags a control style is resolved from.
//!
//! Named fields instead of positional booleans, so a call site cannot swap
//! a `hovered` for a `pressed`.

/// Pointer and enablement state of a control at the moment its style is
/// resolved.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Interaction {
    /// Pointer rests on the control.
    pub hovered: bool,
    /// Pointer is held down on the control.
    pub pressed: bool,
    /// The control is disabled.
    pub disabled: bool,
}

impl Interaction {
    /// No interaction at all.
    pub const IDLE: Self = Self {
        hovered: false,
        pressed: false,
        disabled: false,
    };

    /// The interaction state of `response` in a Ui that is `enabled`.
    #[must_use]
    pub fn of(response: &egui::Response, enabled: bool) -> Self {
        Self {
            hovered: response.hovered(),
            pressed: response.is_pointer_button_down_on(),
            disabled: !enabled,
        }
    }

    /// Idle interaction while measuring, before a response exists.
    #[must_use]
    pub fn measuring(enabled: bool) -> Self {
        Self {
            disabled: !enabled,
            ..Self::IDLE
        }
    }
}
