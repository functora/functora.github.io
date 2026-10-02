use functora_egui::snippet;
use functora_egui::{Flex, ResponsiveExt, ShadcnThemeExt, Typography};

impl crate::state::ShowcaseApp {
    /// Centered fixed-width image tile: image on top, optional caption below.
    ///
    /// The fixed width keeps grid columns aligned regardless of caption
    /// length, and the caption below the image keeps images in a row
    /// top-aligned when captions wrap to different line counts. An empty
    /// caption renders no label, so single-item sections share the exact
    /// same alignment as grid rows. Returns the image response so callers
    /// can detect interactions such as clicks.
    pub fn image_tile(
        ui: &mut egui::Ui,
        width: f32,
        caption: &str,
        image: egui::Image<'_>,
    ) -> egui::Response {
        ui.set_min_width(width);
        ui.set_max_width(width);
        ui.vertical_centered(|centered| {
            let response = centered.add(image);
            if !caption.is_empty() {
                _ = Typography::small(caption).show(centered);
            }
            response
        })
        .inner
    }

    pub(crate) fn demo_image(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Responsive image widget with SVG, PNG, JPEG, GIF, WebP, BMP, ICO, QOI, Farbfeld, TIFF and PPM support.",
        )
        .show(ui);
        _ = Typography::muted(
            "Two fixture variants: opaque RGB images, and images with an alpha channel whose light cells are transparent (marked alpha below).",
        )
        .show(ui);
        ui.add_space(12.0);

        match functora_egui::image_samples() {
            Ok(samples) => self.demo_image_samples(ui, samples),
            Err(error) => {
                _ = Typography::muted(format!("Sample images unavailable: {error:?}")).show(ui);
                ui.add_space(12.0);
            }
        }
        Self::demo_image_snippet(ui);
    }

    pub(crate) fn demo_image_samples(
        &mut self,
        ui: &mut egui::Ui,
        samples: &functora_egui::ImageSamples,
    ) {
        let png = samples.png.to_image();
        let transparent = samples.transparent.to_image();
        let theme = ui.ctx().shadcn_theme();
        let spacing = ui.responsive_spacing();
        let cell = if ui.on_mobile() { 96.0 } else { 140.0 };

        _ = Typography::h4("Formats").show(ui);
        _ = Typography::muted("Same motif in every supported encoding.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                for (label, sample) in samples
                    .all()
                    .into_iter()
                    .chain([("Transparent PNG", &samples.transparent)])
                {
                    let caption = if sample.has_alpha {
                        format!("{label} (alpha)")
                    } else {
                        label.to_owned()
                    };
                    _ = f.ui(|ui2| {
                        _ = Self::image_tile(
                            ui2,
                            cell + 24.0,
                            &caption,
                            sample.to_image().max_width(cell).alt_text(&caption),
                        );
                    });
                }
            });
        ui.add_space(16.0);

        _ = Typography::h4("Fit modes").show(ui);
        _ = Typography::muted("Contain, cover, fixed width, and stretched filling.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "Contain",
                        png.clone()
                            .maintain_aspect_ratio(true)
                            .max_width(200.0)
                            .max_height(120.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "Cover",
                        png.clone()
                            .maintain_aspect_ratio(true)
                            .fit_to_exact_size(egui::vec2(200.0, 120.0)),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "FitWidth",
                        png.clone().maintain_aspect_ratio(true).max_width(200.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "Fill (distorts)",
                        png.clone()
                            .maintain_aspect_ratio(false)
                            .fit_to_exact_size(egui::vec2(200.0, 120.0)),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("bg_fill (alpha image over theme color)").show(ui);
        _ = Typography::muted(
            "Transparent cells show the fill color through; the blue cells and red circle stay opaque.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "On primary",
                        transparent.clone().bg_fill(theme.primary).max_width(60.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "On destructive",
                        transparent
                            .clone()
                            .bg_fill(theme.destructive)
                            .max_width(60.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "On accent",
                        transparent.clone().bg_fill(theme.accent).max_width(60.0),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("tint (colorize, alpha image)").show(ui);
        _ = Typography::muted("Tint multiplies every pixel, transparent cells included.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "Primary tint",
                        transparent.clone().tint(theme.primary).max_width(60.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "Destructive tint",
                        transparent.clone().tint(theme.destructive).max_width(60.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        84.0,
                        "Accent tint",
                        transparent.clone().tint(theme.accent).max_width(60.0),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("uv (sub-region)").show(ui);
        _ = Typography::muted("Normalized coordinates crop out a sub-region.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(ui2, 144.0, "Full", png.clone().max_width(120.0));
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        144.0,
                        "Top-left quadrant",
                        png.clone()
                            .uv(egui::Rect::from_min_max(
                                egui::pos2(0.0, 0.0),
                                egui::pos2(0.5, 0.5),
                            ))
                            .max_width(120.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        144.0,
                        "Bottom-right quadrant",
                        png.clone()
                            .uv(egui::Rect::from_min_max(
                                egui::pos2(0.5, 0.5),
                                egui::pos2(1.0, 1.0),
                            ))
                            .max_width(120.0),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("sense (clickable)").show(ui);
        _ = Typography::muted("Click the image to fire a toast.").show(ui);
        ui.add_space(8.0);
        let sense_ctx = ui.ctx().clone();
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    if Self::image_tile(
                        ui2,
                        144.0,
                        "",
                        png.clone()
                            .sense(egui::Sense::click())
                            .fit_to_exact_size(egui::vec2(120.0, 120.0)),
                    )
                    .clicked()
                    {
                        self.toast.add(
                            "Image clicked",
                            functora_egui::ToastVariant::Default,
                            sense_ctx.input(|i| i.time),
                        );
                    }
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("rotate (45-deg about center)").show(ui);
        _ = Typography::muted(
            "Rotation around a relative origin point; the tilted corners need breathing room.",
        )
        .show(ui);
        ui.add_space(8.0);
        ui.add_space(20.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        144.0,
                        "",
                        png.clone()
                            .rotate(std::f32::consts::FRAC_PI_4, egui::vec2(0.5, 0.5))
                            .fit_to_exact_size(egui::vec2(80.0, 80.0)),
                    );
                });
            });
        ui.add_space(20.0);

        _ = Typography::h4("texture_options (filtering)").show(ui);
        _ = Typography::muted("Linear smooths magnified pixels; nearest keeps them crisp.")
            .show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        144.0,
                        "Linear",
                        png.clone()
                            .texture_options(egui::TextureOptions::LINEAR)
                            .max_width(120.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        144.0,
                        "Nearest",
                        png.clone()
                            .texture_options(egui::TextureOptions::NEAREST)
                            .max_width(120.0),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("fit_to_fraction / shrink_to_fit").show(ui);
        _ = Typography::muted(
            "Fractional sizing needs real available height, so these live in a row; capped so wide viewports stay sane.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        204.0,
                        "fit_to_fraction(0.5)",
                        png.clone()
                            .fit_to_fraction(egui::vec2(0.5, 0.5))
                            .max_size(egui::vec2(280.0, 180.0)),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "shrink_to_fit()",
                        png.clone()
                            .shrink_to_fit()
                            .max_size(egui::vec2(320.0, 200.0)),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("from_texture").show(ui);
        _ = Typography::muted("Wrap a pre-loaded GPU texture instead of bytes.").show(ui);
        ui.add_space(8.0);
        {
            let texture = ui.ctx().load_texture(
                "demo-image-texture",
                egui::ColorImage::example(),
                egui::TextureOptions::LINEAR,
            );
            _ = Flex::row()
                .gap(spacing.gap)
                .align_start()
                .wrap()
                .show(ui, |f| {
                    _ = f.ui(|ui2| {
                        _ = Self::image_tile(
                            ui2,
                            144.0,
                            "",
                            egui::Image::from_texture(egui::load::SizedTexture::from_handle(
                                &texture,
                            ))
                            .max_width(120.0),
                        );
                    });
                });
        }
        ui.add_space(16.0);

        _ = Typography::h4("shimmer (loading spinner)").show(ui);
        _ = Typography::muted("Spinner placeholder while the bytes stream in.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        104.0,
                        "Shimmer on",
                        png.clone().show_loading_spinner(true).max_width(80.0),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        104.0,
                        "Shimmer off",
                        png.clone().show_loading_spinner(false).max_width(80.0),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("Responsive (view-adaptive width)").show(ui);
        _ = Typography::muted("Width follows the viewport with a mobile-aware cap.").show(ui);
        ui.add_space(8.0);
        let viewport_max = if ui.on_mobile() { 160.0 } else { 400.0 };
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        viewport_max + 24.0,
                        "",
                        png.clone()
                            .maintain_aspect_ratio(true)
                            .fit_to_exact_size(egui::vec2(viewport_max, viewport_max)),
                    );
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("max_size & width").show(ui);
        _ = Typography::muted("Upper bounds that never force an exact size.").show(ui);
        ui.add_space(8.0);
        _ = Flex::row()
            .gap(spacing.gap)
            .align_start()
            .wrap()
            .show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(
                        ui2,
                        224.0,
                        "max_size(200,150)",
                        png.clone().max_size(egui::vec2(200.0, 150.0)),
                    );
                });
                _ = f.ui(|ui2| {
                    _ = Self::image_tile(ui2, 224.0, "width(140)", png.clone().max_width(140.0));
                });
            });
        ui.add_space(16.0);

        _ = Typography::h4("Error state").show(ui);
        _ = Typography::muted(
            "Failed loads size to a 24 px fallback box, so pin an exact size with aspect maintenance off to leave room for the alt text.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = ui.add(
            egui::Image::from_bytes("bytes://missing.png", b"corrupt".to_vec())
                .maintain_aspect_ratio(false)
                .fit_to_exact_size(egui::vec2(300.0, 64.0))
                .alt_text("Blue checkerboard, red circle"),
        );
        ui.add_space(12.0);
    }

    pub(crate) fn demo_image_snippet(ui: &mut egui::Ui) {
        snippet(
            ui,
            "// Image: responsive images with egui::Image\n\n// Setup once alongside fonts (enables svg, file and http loaders)\nfunctora_egui::setup_image_loaders(&cc.egui_ctx);\n\n// Show SVG, PNG, JPEG, GIF, WebP, BMP, ICO, QOI, Farbfeld, TIFF or PPM from bytes\n// Keep the file extension in the URI so the loader routes correctly.\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .maintain_aspect_ratio(true)\n        .max_width(300.0)\n        .alt_text(\"A photo\"),\n);\n\n// Cover mode crops overflow while preserving aspect\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .maintain_aspect_ratio(true)\n        .fit_to_exact_size(egui::vec2(300.0, 200.0)),\n);\n\n// Paint a transparent image over a theme color (needs an alpha-channel image:
// bg_fill shows through transparent pixels while opaque pixels cover it)\nui.add(\n    egui::Image::from_bytes(\"bytes://logo.png\", logo_bytes)\n        .bg_fill(theme.primary)\n        .max_width(48.0),\n);\n\n// Colorize an image\nui.add(\n    egui::Image::from_bytes(\"bytes://avatar.png\", avatar_bytes)\n        .tint(theme.primary)\n        .max_width(48.0),\n);\n\n// Clickable image\nif ui\n    .add(\n        egui::Image::from_bytes(\"bytes://preview.png\", data).sense(egui::Sense::click()),\n    )\n    .clicked()\n{\n    // open lightbox\n}\n\n// Rotate about a relative origin (leave room: tilted corners extend past the box)\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .rotate(std::f32::consts::FRAC_PI_4, egui::vec2(0.5, 0.5))\n        .fit_to_exact_size(egui::vec2(80.0, 80.0)),\n);\n\n// Fractional sizing needs real available height, so show it in a row;\n// cap it so wide viewports stay sane\nfunctora_egui::Flex::row().gap(8.0).show(ui, |f| {\n    f.ui(|tile| {\n        tile.add(\n            egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n                .fit_to_fraction(egui::vec2(0.5, 0.5))\n                .max_size(egui::vec2(280.0, 180.0)),\n        );\n    });\n});\n\n// View-adaptive width (exact box: bare max_width collapses without available height)\nlet viewport_max = if ui.on_mobile() { 160.0 } else { 400.0 };\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .maintain_aspect_ratio(true)\n        .fit_to_exact_size(egui::vec2(viewport_max, viewport_max)),\n);\n\n// Responsive by default; respects available width on mobile\nui.add(egui::Image::from_uri(\"https://example.com/photo.webp\"));\n\n// Bundled fixtures: ImageSamples carries every encoding plus a transparent PNG\nlet samples = functora_egui::image_samples().expect(\"fixtures must encode\");\n\n// to_image bakes in the routing URI; clone one builder across many widgets\nlet png = samples.png.to_image();\nui.add(\n    png.clone()\n        .maintain_aspect_ratio(true)\n        .max_width(300.0)\n        .alt_text(\"Blue and sky checkerboard with a red circle\"),\n);\n\n// Transparent fixture: the fill shows through transparent pixels\nui.add(\n    samples\n        .transparent\n        .to_image()\n        .bg_fill(theme.secondary)\n        .max_width(160.0),\n);\n\n// Error state: failed loads fall back to a 24 px box, so pin an exact\n// size with aspect maintenance off to leave room for the alt text\nui.add(\n    egui::Image::from_bytes(\"bytes://missing.png\", data)\n        .maintain_aspect_ratio(false)\n        .fit_to_exact_size(egui::vec2(300.0, 64.0))\n        .alt_text(\"Blue checkerboard, red circle\"),\n);",
        );
    }
}
