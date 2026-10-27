//! Label geometry shared by the labeled controls (checkbox, radio, switch):
//! one laid-out label beside a fixed-size control, with the intrinsic
//! (non-wrapped) sizes flex containers record as the control's slot.

pub(crate) struct ControlLabel {
    galley: Option<std::sync::Arc<egui::Galley>>,
    gap: f32,
}

impl ControlLabel {
    /// Lays `label` out into the room left beside a `control_width` wide
    /// control, in the shared 14pt control-label font.
    pub(crate) fn new(
        ui: &egui::Ui,
        label: Option<egui::WidgetText>,
        color: egui::Color32,
        control_width: f32,
        gap: f32,
    ) -> Self {
        Self {
            galley: label.map(|l| {
                crate::text::layout(
                    ui,
                    l.text().to_owned(),
                    egui::FontId::proportional(14.0),
                    color,
                    (ui.available_width() - control_width - gap).max(0.0),
                    None,
                )
            }),
            gap,
        }
    }

    /// Width the laid-out label adds to its control, gap included.
    pub(crate) fn width(&self) -> f32 {
        self.galley.as_ref().map_or(0.0, |g| g.size().x + self.gap)
    }

    /// Height the laid-out label needs.
    pub(crate) fn height(&self) -> f32 {
        self.galley.as_ref().map_or(0.0, |g| g.size().y)
    }

    /// Width the label adds at its intrinsic (non-wrapped) size.
    pub(crate) fn intrinsic_width(&self) -> f32 {
        self.galley
            .as_ref()
            .map_or(0.0, |g| g.intrinsic_size().x + self.gap)
    }

    /// Height the label needs at its intrinsic (non-wrapped) size.
    pub(crate) fn intrinsic_height(&self) -> f32 {
        self.galley.as_ref().map_or(0.0, |g| g.intrinsic_size().y)
    }

    /// Paints the label vertically centered in `row`, `gap` right of the
    /// control's `control_end` edge.
    pub(crate) fn paint(
        &self,
        painter: &egui::Painter,
        control_end: f32,
        row: egui::Rect,
        fallback_color: egui::Color32,
    ) {
        if let Some(galley) = self.galley.as_ref() {
            painter.galley(
                egui::pos2(
                    control_end + self.gap,
                    row.center().y - galley.size().y / 2.0,
                ),
                std::sync::Arc::clone(galley),
                fallback_color,
            );
        }
    }
}
