//! Installs egui's built-in image loaders (png, jpeg, gif, webp, svg, bmp, ico, qoi, farbfeld, tiff, ppm).

/// Install egui image loaders once during app setup.
///
/// Call this in your `CreationContext` callback alongside [`setup_fonts`]:
///
/// ```ignore
/// functora_egui::setup_image_loaders(&cc.egui_ctx);
/// ```
#[cfg(feature = "images")]
pub fn setup_image_loaders(ctx: &egui::Context) {
    egui_extras::install_image_loaders(ctx);
    #[cfg(feature = "markdown")]
    crate::markdown_loader::install_data_url_loader(ctx);
}

#[cfg(all(not(feature = "images"), feature = "markdown"))]
pub fn setup_image_loaders(ctx: &egui::Context) {
    crate::markdown_loader::install_data_url_loader(ctx);
}

#[cfg(all(not(feature = "images"), not(feature = "markdown")))]
pub fn setup_image_loaders(_ctx: &egui::Context) {}
