//! Widget size variants matching shadcn/ui's Nova size system.

/// Size variants for components like Button and Toggle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ComponentSize {
    /// Extra small: h=24, px=8, text=12
    Xs,
    /// Small: h=28, px=10, text=12.8
    Sm,
    /// Default: h=32, px=10, text=14
    #[default]
    Default,
    /// Large: h=36, px=10, text=14
    Lg,
}

impl ComponentSize {
    /// Returns (height, horizontal_padding, font_size) in logical pixels.
    pub fn metrics(self) -> (f32, f32, f32) {
        match self {
            Self::Xs => (24.0, 8.0, 12.0),
            Self::Sm => (28.0, 10.0, 12.8),
            Self::Default => (32.0, 10.0, 14.0),
            Self::Lg => (36.0, 10.0, 14.0),
        }
    }

    /// Returns (height, horizontal_padding, font_size) scaled to the given
    /// responsive spacing: mobile viewports use touch-friendly heights while
    /// font sizes stay identical so text looks uniform across devices.
    pub fn metrics_for(self, spacing: &crate::responsive::Spacing) -> (f32, f32, f32) {
        let (height, h_padding, font_size) = self.metrics();
        if spacing.is_mobile() {
            (
                height + spacing.touch_height - Self::Default.metrics().0,
                h_padding + spacing.touch_padding - 10.0,
                font_size,
            )
        } else {
            (height, h_padding, font_size)
        }
    }
}
