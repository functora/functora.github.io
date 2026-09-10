//! Display: typography, labels, keyboard hints, items, and the icon catalog.

use functora_egui::{
    Button, ButtonVariant, Card, Flex, Item, Kbd, Label, LucideIcon, ResponsiveExt, ScrollArea,
    Separator, ShadcnThemeExt, Typography, TypographyVariant,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_typography(ui: &mut egui::Ui) {
        _ = Typography::muted("Text styles: headings, lead, muted, and small.").show(ui);
        ui.add_space(12.0);
        _ = Typography::h1("The Joke Tax Chronicles").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "Once upon a time, in a far-off land, there was a very lazy king who spent all day \
             lounging on his throne. One day, his advisors came to him with a problem.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h2("The King's Plan").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "The king thought long and hard, and finally came up with a brilliant plan.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h3("The Joke").show(ui);
        ui.add_space(4.0);
        _ = Typography::new("Why did the chicken cross the road? To get to the other side.")
            .show(ui);
        ui.add_space(8.0);
        _ = Typography::h4("People stopped telling jokes").show(ui);
        ui.add_space(4.0);
        _ = Typography::small("The moral of the story is: this is a typography demo.").show(ui);
        ui.add_space(8.0);
        _ = Typography::lead("This is a lead paragraph: slightly larger and muted.").show(ui);
        ui.add_space(8.0);
        _ = Typography::muted("Muted text is dimmer for secondary content.").show(ui);
        ui.add_space(8.0);
        _ = Typography::new("Plain paragraph style with a custom variant.")
            .variant(TypographyVariant::Large)
            .show(ui);

        snippet(
            ui,
            "// Typography: styled text with variants\nuse functora_egui::{Typography, TypographyVariant};\n\nTypography::h1(\"Title\").show(ui);\nTypography::h2(\"Subtitle\").show(ui);\nTypography::h3(\"Heading\").show(ui);\nTypography::h4(\"Sub-heading\").show(ui);\nTypography::small(\"fine print\").show(ui);\nTypography::lead(\"Lead paragraph\").show(ui);\nTypography::muted(\"Muted text\").show(ui);\n\n// Custom variant\nTypography::new(\"Plain paragraph with Large variant.\")\n    .variant(TypographyVariant::Large)\n    .show(ui);",
        );
    }

    pub fn demo_label(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Labels pair with inputs in forms and settings.").show(ui);
        ui.add_space(12.0);
        _ = Label::new("Your email address").show(ui);
        ui.add_space(8.0);
        _ = functora_egui::Input::new(&mut self.flex_email)
            .placeholder("you@example.com")
            .show(ui);
        ui.add_space(8.0);
        _ = Label::new("Sizes").show(ui);
        ui.add_space(8.0);
        _ = Label::new("Small label")
            .size(functora_egui::ComponentSize::Sm)
            .show(ui);
        ui.add_space(8.0);
        _ = Label::new("Muted label").muted().show(ui);

        snippet(
            ui,
            "// Label: text labels for forms\nuse functora_egui::{Label, Input, ComponentSize, Flex};\n\nLabel::new(\"Your email address\").show(ui);\n\nInput::new(&mut email).placeholder(\"you@example.com\").show(ui);\n\n// Sizes\nLabel::new(\"Small label\").size(ComponentSize::Sm).show(ui);\nLabel::new(\"Muted label\").muted().show(ui);",
        );
    }

    pub(crate) fn demo_kbd(ui: &mut egui::Ui) {
        _ = Typography::muted("Keyboard hint chips for shortcuts.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Ctrl"));
            _ = f.ui(|ui55| {
                _ = ui55.label("+");
            });
            _ = f.add(Kbd::new("K"));
            _ = f.ui(|ui56| {
                _ = ui56.label("opens the command palette");
            });
        });
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Shift"));
            _ = f.ui(|ui57| {
                _ = ui57.label("+");
            });
            _ = f.add(Kbd::new("Tab"));
            _ = f.ui(|ui58| {
                _ = ui58.label("cycles focus");
            });
        });

        snippet(
            ui,
            "// Kbd: keyboard hint chips\nuse functora_egui::{Kbd, Flex};\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Ctrl\"));\n    f.label(\"+\");\n    f.add(Kbd::new(\"K\"));\n    f.label(\"opens command palette\");\n});\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Shift\"));\n    f.label(\"+\");\n    f.add(Kbd::new(\"Tab\"));\n    f.label(\"cycles focus\");\n});",
        );
    }

    pub(crate) fn demo_item(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable rows for lists and menus.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Default variant").show(ui);
        ui.add_space(4.0);
        for (title, desc) in [
            ("Notifications", "Check your activity and updates"),
            ("Appearance", "Choose a theme for the app"),
            ("Storage", "Manage files and downloads"),
        ] {
            if Item::new()
                .show(ui, |ui17| {
                    _ = ui17.vertical(|ui18| {
                        _ = Label::new(title).show(ui18);
                        _ = ui18.label(desc);
                    });
                })
                .clicked()
            {
                self.toast.add(
                    format!("Item: {title}"),
                    functora_egui::ToastVariant::Default,
                    ui.ctx().input(|i| i.time),
                );
            }
        }
        ui.add_space(12.0);
        _ = Typography::small("Outline variant with icons").show(ui);
        ui.add_space(4.0);
        if Item::new()
            .variant(functora_egui::ItemVariant::Outline)
            .show(ui, |ui19| {
                _ = ui19.horizontal(|ui20| {
                    _ = Button::icon_only(LucideIcon::Settings)
                        .variant(ButtonVariant::Ghost)
                        .size(functora_egui::ComponentSize::Sm)
                        .show(ui20);
                    _ = ui20.label("Open settings");
                });
            })
            .clicked()
        {
            self.toast.add(
                "Settings",
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// Item: clickable rows for lists/menus\nuse functora_egui::{Item, Label, Button, LucideIcon, ButtonVariant, ComponentSize};\n\n// Default variant\nItem::new().show(ui, |item| {\n    item.vertical(|v| {\n        v.add(Label::new(\"Notifications\"));\n        v.label(\"Check your activity and updates\");\n    });\n});\n\n// Outline variant with icons\nItem::new().variant(ItemVariant::Outline).show(ui, |item| {\n    item.horizontal(|h| {\n        h.add(Button::icon_only(LucideIcon::Settings).variant(ButtonVariant::Ghost).size(ComponentSize::Sm));\n        h.label(\"Open settings\");\n    });\n});",
        );
    }

    pub(crate) fn demo_icons(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("All 1600+ Lucide icons are available with one import.").show(ui);
        ui.add_space(12.0);
        _ = functora_egui::Input::new(&mut self.icon_search)
            .placeholder("Search icons...")
            .desired_width(260.0)
            .show(ui);
        ui.add_space(8.0);
        let needle = self.icon_search.trim().to_ascii_lowercase();
        let icons: Vec<LucideIcon> = functora_egui::icons::lucide_icon::ALL
            .iter()
            .copied()
            .filter(|icon| needle.is_empty() || icon.name().to_ascii_lowercase().contains(&needle))
            .collect();
        _ = Typography::small(format!("{} icons", icons.len())).show(ui);
        ui.add_space(6.0);
        _ = ScrollArea::new(320.0).show(ui, |ui21| {
            _ = ui21.horizontal_wrapped(|ui22| {
                for icon in icons {
                    if ui22
                        .add(
                            Button::icon_only(icon)
                                .variant(ButtonVariant::Ghost)
                                .size(functora_egui::ComponentSize::Sm),
                        )
                        .on_hover_text(icon.name())
                        .clicked()
                    {
                        self.toast.add(
                            icon.name(),
                            functora_egui::ToastVariant::Default,
                            ui22.ctx().input(|i| i.time),
                        );
                    }
                }
            });
        });
        ui.add_space(12.0);
        _ = Separator::horizontal().show(ui);
        ui.add_space(4.0);
        _ = Typography::small("Icons render from built-in SVG paths; no external font needed.")
            .show(ui);

        snippet(
            ui,
            "// Icons: 1600+ Lucide icons from built-in SVG paths\nuse functora_egui::{LucideIcon, Button, ButtonVariant, ComponentSize};\n\n// Search and display icons\nlet search = \"settings\";\nlet icons: Vec<LucideIcon> = LucideIcon::ALL\n    .iter()\n    .copied()\n    .filter(|icon| icon.name().to_lowercase().contains(search))\n    .collect();\n\nfor icon in icons {\n    Button::icon_only(icon)\n        .variant(ButtonVariant::Ghost)\n        .size(ComponentSize::Sm)\n        .on_hover_text(icon.name())\n        .show(ui);\n}\n\n// Icons render from built-in SVG; no external font needed",
        );
    }

    pub(crate) fn demo_image(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Responsive image widget with SVG, PNG, JPEG, GIF, WebP, BMP, ICO, QOI, Farbfeld, TIFF and PPM support.",
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
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            for (label, sample) in samples.all() {
                _ = f.ui(|ui2| {
                    _ = Typography::small(label).show(ui2);
                    _ = ui2.add(sample.to_image().max_width(cell).alt_text(label));
                });
            }
        });
        ui.add_space(16.0);

        _ = Typography::h4("Fit modes").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("Contain").show(ui2);
                _ = ui2.add(
                    png.clone()
                        .maintain_aspect_ratio(true)
                        .max_width(200.0)
                        .max_height(120.0),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Cover").show(ui2);
                _ = ui2.add(
                    png.clone()
                        .maintain_aspect_ratio(true)
                        .fit_to_exact_size(egui::vec2(200.0, 120.0)),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("FitWidth").show(ui2);
                _ = ui2.add(png.clone().maintain_aspect_ratio(true).max_width(200.0));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Fill (distorts)").show(ui2);
                _ = ui2.add(png.clone().fit_to_exact_size(egui::vec2(200.0, 120.0)));
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("bg_fill (transparent images)").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("On primary").show(ui2);
                _ = ui2.add(transparent.clone().bg_fill(theme.primary).max_width(60.0));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("On destructive").show(ui2);
                _ = ui2.add(
                    transparent
                        .clone()
                        .bg_fill(theme.destructive)
                        .max_width(60.0),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("On accent").show(ui2);
                _ = ui2.add(transparent.clone().bg_fill(theme.accent).max_width(60.0));
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("tint (colorize)").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("Primary tint").show(ui2);
                _ = ui2.add(transparent.clone().tint(theme.primary).max_width(60.0));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Destructive tint").show(ui2);
                _ = ui2.add(
                    transparent
                        .clone()
                        .tint(theme.destructive)
                        .max_width(60.0),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Accent tint").show(ui2);
                _ = ui2.add(transparent.clone().tint(theme.accent).max_width(60.0));
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("uv (sub-region)").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("Full").show(ui2);
                _ = ui2.add(png.clone().max_width(120.0));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Top-left quadrant").show(ui2);
                _ = ui2.add(
                    png.clone()
                        .uv(egui::Rect::from_min_max(
                            egui::pos2(0.0, 0.0),
                            egui::pos2(0.5, 0.5),
                        ))
                        .max_width(120.0),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Bottom-right quadrant").show(ui2);
                _ = ui2.add(
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
        ui.add_space(8.0);
        if ui
            .add(png.clone().sense(egui::Sense::click()).max_width(120.0))
            .clicked()
        {
            self.toast.add(
                "Image clicked",
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(16.0);

        _ = Typography::h4("rotate (45-deg about center)").show(ui);
        ui.add_space(8.0);
        _ = ui.add(
            png.clone()
                .rotate(std::f32::consts::FRAC_PI_4, egui::vec2(0.5, 0.5))
                .max_width(120.0),
        );
        ui.add_space(16.0);

        _ = Typography::h4("texture_options (filtering)").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("Linear").show(ui2);
                _ = ui2.add(
                    png.clone()
                        .texture_options(egui::TextureOptions::LINEAR)
                        .max_width(120.0),
                );
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Nearest").show(ui2);
                _ = ui2.add(
                    png.clone()
                        .texture_options(egui::TextureOptions::NEAREST)
                        .max_width(120.0),
                );
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("fit_to_fraction / shrink_to_fit").show(ui);
        ui.add_space(8.0);
        _ = ui.add(png.clone().fit_to_fraction(egui::vec2(0.5, 0.5)));
        ui.add_space(8.0);
        _ = ui.add(png.clone().shrink_to_fit());
        ui.add_space(16.0);

        _ = Typography::h4("from_texture").show(ui);
        ui.add_space(8.0);
        {
            let texture = ui.ctx().load_texture(
                "demo-image-texture",
                egui::ColorImage::example(),
                egui::TextureOptions::LINEAR,
            );
            _ = ui.add(
                egui::Image::from_texture(egui::load::SizedTexture::from_handle(&texture))
                    .max_width(120.0),
            );
        }
        ui.add_space(16.0);

        _ = Typography::h4("shimmer (loading spinner)").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("Shimmer on").show(ui2);
                _ = ui2.add(png.clone().show_loading_spinner(true).max_width(80.0));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("Shimmer off").show(ui2);
                _ = ui2.add(png.clone().show_loading_spinner(false).max_width(80.0));
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("Responsive (view-adaptive width)").show(ui);
        ui.add_space(8.0);
        let viewport_max = if ui.on_mobile() { 160.0 } else { 400.0 };
        _ = ui.add(
            png.clone()
                .maintain_aspect_ratio(true)
                .max_width(viewport_max),
        );
        ui.add_space(16.0);

        _ = Typography::h4("max_size & width").show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(spacing.gap).wrap().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small("max_size(200,150)").show(ui2);
                _ = ui2.add(png.clone().max_size(egui::vec2(200.0, 150.0)));
            });
            _ = f.ui(|ui2| {
                _ = Typography::small("width(140)").show(ui2);
                _ = ui2.add(png.clone().max_width(140.0));
            });
        });
        ui.add_space(16.0);

        _ = Typography::h4("Card + Image").show(ui);
        ui.add_space(8.0);
        _ = Card::new().heading("Image inside a Card").show(ui, |ui2| {
            _ = ui2.add(transparent.clone().bg_fill(theme.secondary).max_width(160.0));
            ui2.add_space(8.0);
            _ = Typography::small("Images compose inside cards.").show(ui2);
        });
        ui.add_space(16.0);

        _ = Typography::small("Error state shows alt text.").show(ui);
        ui.add_space(4.0);
        _ = ui.add(
            egui::Image::from_bytes("bytes://missing.png", b"corrupt".to_vec())
                .alt_text("Image unavailable")
                .max_width(120.0),
        );
        ui.add_space(12.0);
    }

    pub(crate) fn demo_image_snippet(ui: &mut egui::Ui) {
        snippet(
            ui,
            "// Image: responsive images with egui::Image\n\n// Setup once alongside fonts (enables svg, file and http loaders)\nfunctora_egui::setup_image_loaders(&cc.egui_ctx);\n\n// Show SVG, PNG, JPEG, GIF, WebP, BMP, ICO, QOI, Farbfeld, TIFF or PPM from bytes\n// Keep the file extension in the URI so the loader routes correctly.\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .maintain_aspect_ratio(true)\n        .max_width(300.0)\n        .alt_text(\"A photo\"),\n);\n\n// Cover mode crops overflow while preserving aspect\nui.add(\n    egui::Image::from_bytes(\"bytes://photo.png\", photo_bytes)\n        .maintain_aspect_ratio(true)\n        .fit_to_exact_size(egui::vec2(300.0, 200.0)),\n);\n\n// Paint a transparent image over a theme color\nui.add(\n    egui::Image::from_bytes(\"bytes://logo.png\", logo_bytes)\n        .bg_fill(theme.primary)\n        .max_width(48.0),\n);\n\n// Colorize an image\nui.add(\n    egui::Image::from_bytes(\"bytes://avatar.png\", avatar_bytes)\n        .tint(theme.primary)\n        .max_width(48.0),\n);\n\n// Clickable image\nif ui\n    .add(\n        egui::Image::from_bytes(\"bytes://preview.png\", data).sense(egui::Sense::click()),\n    )\n    .clicked()\n{\n    // open lightbox\n}\n\n// Responsive by default; respects available width on mobile\nui.add(egui::Image::from_uri(\"https://example.com/photo.webp\"));",
        );
    }
}
