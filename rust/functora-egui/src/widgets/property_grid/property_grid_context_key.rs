//! Temporary data key for nested property grid rows.

pub(crate) fn property_grid_context_key() -> egui::Id {
    egui::Id::unique("functora_egui_property_grid_context")
}
