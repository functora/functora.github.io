use functora_egui::snippet;
use functora_egui::{
    Button, ButtonVariant, Card, Flex, ResponsiveExt, ShadcnTheme, ShadcnThemeExt, Spacing,
    Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_utilities(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Utilities: the public helper modules - cast (saturating numeric conversions), frame (camera buffers), text (measurement), paint (shared painting), utils (compatibility facade), tokens (design tokens), config (build metadata), icon_export and build.",
        )
        .show(ui);
        ui.add_space(12.0);

        let packaging = self.platform.packaging.metadata();

        crate::screens::section(ui, "cast: saturating numeric conversions");
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for (label, value) in [
                    (
                        "f32_to_u8_clamped(300.0)",
                        functora_egui::cast::f32_to_u8_clamped(300.0).to_string(),
                    ),
                    (
                        "f32_to_usize_clamped(-4.0)",
                        functora_egui::cast::f32_to_usize_clamped(-4.0).to_string(),
                    ),
                    (
                        "usize_to_f32(7)",
                        functora_egui::cast::usize_to_f32(7).to_string(),
                    ),
                    (
                        "i32_to_f32(-3)",
                        functora_egui::cast::i32_to_f32(-3).to_string(),
                    ),
                    (
                        "f64_to_f32(1.5)",
                        functora_egui::cast::f64_to_f32(1.5).to_string(),
                    ),
                    (
                        "f64_to_i32(2.9)",
                        functora_egui::cast::f64_to_i32(2.9).to_string(),
                    ),
                    (
                        "usize_to_u32(usize::MAX)",
                        functora_egui::cast::usize_to_u32(usize::MAX).to_string(),
                    ),
                    (
                        "f64_to_u64_clamped(-1.0)",
                        functora_egui::cast::f64_to_u64_clamped(-1.0).to_string(),
                    ),
                    (
                        "u64_to_f64_js(9)",
                        functora_egui::cast::u64_to_f64_js(9).to_string(),
                    ),
                    (
                        "u32_to_f32(12)",
                        functora_egui::cast::u32_to_f32(12).to_string(),
                    ),
                    (
                        "cap_to_available(300.0, 200.0)",
                        functora_egui::cast::cap_to_available(300.0, 200.0).to_string(),
                    ),
                    (
                        "scaled_px(24, 2.0)",
                        functora_egui::cast::scaled_px(24, 2.0).to_string(),
                    ),
                ] {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{label} = {value}")).show(ui3);
                    });
                }
            });
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "frame: camera buffers and preview sizing");
        let luma = vec![16_u8; 16];
        let rgba = vec![255_u8; 64];
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for (label, value) in [
                    (
                        "pixel_area_len(4, 4)",
                        functora_egui::frame::pixel_area_len(4, 4).to_string(),
                    ),
                    (
                        "FrameRotation::from_degrees_cw(90)",
                        format!(
                            "{:?}",
                            functora_egui::frame::FrameRotation::from_degrees_cw(90)
                        ),
                    ),
                    (
                        "rotated_dimensions(4, 2, Cw90)",
                        format!(
                            "{:?}",
                            functora_egui::frame::rotated_dimensions(
                                4,
                                2,
                                functora_egui::frame::FrameRotation::Cw90
                            )
                        ),
                    ),
                    (
                        "rotate_luma(4x2).len",
                        functora_egui::frame::rotate_luma(
                            &luma,
                            4,
                            2,
                            functora_egui::frame::FrameRotation::Cw90,
                        )
                        .0
                        .len()
                        .to_string(),
                    ),
                    (
                        "rotate_rgba(4x2).len",
                        functora_egui::frame::rotate_rgba(
                            &rgba,
                            4,
                            2,
                            functora_egui::frame::FrameRotation::Cw180,
                        )
                        .0
                        .len()
                        .to_string(),
                    ),
                    (
                        "responsive_preview_size(200, 100, 800, 600, 4, 2)",
                        format!(
                            "{:?}",
                            functora_egui::frame::responsive_preview_size(
                                200.0, 100.0, 800.0, 600.0, 4, 2
                            )
                        ),
                    ),
                    (
                        "fit_preview_size(100x50, 4, 2)",
                        format!(
                            "{:?}",
                            functora_egui::frame::fit_preview_size(egui::vec2(100.0, 50.0), 4, 2)
                        ),
                    ),
                    (
                        "nv21_luma(4x2).len",
                        functora_egui::frame::nv21_luma(&rgba, 4, 2)
                            .len()
                            .to_string(),
                    ),
                    (
                        "nv21_to_rgba(4x2).len",
                        functora_egui::frame::nv21_to_rgba(&rgba, 4, 2)
                            .len()
                            .to_string(),
                    ),
                    (
                        "fps_to_interval_ms(30.0)",
                        functora_egui::frame::fps_to_interval_ms(30.0).to_string(),
                    ),
                    (
                        "RESPONSIVE_PREVIEW_MAX_SIDE / FALLBACK_WIDTH / FALLBACK_ASPECT",
                        format!(
                            "{} / {} / {}",
                            functora_egui::frame::RESPONSIVE_PREVIEW_MAX_SIDE,
                            functora_egui::frame::RESPONSIVE_PREVIEW_FALLBACK_WIDTH,
                            functora_egui::frame::RESPONSIVE_PREVIEW_FALLBACK_ASPECT
                        ),
                    ),
                ] {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{label} = {value}")).show(ui3);
                    });
                }
            });
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "text: measurement and wrapping");
        let font_id = egui::TextStyle::Body.resolve(ui.style());
        let galley = functora_egui::text::layout(
            ui,
            "functora-egui lays text out inside the box it is given",
            font_id.clone(),
            ui.visuals().text_color(),
            120.0,
            Some(functora_egui::text::Wrap::Word),
        );
        let wrap = functora_egui::text::resolve(ui, None);
        let natural_width = functora_egui::text::natural_width(ui.ctx(), "functora-egui", font_id);
        let box_height = functora_egui::text::box_height(&galley, 24.0);
        let intrinsic_box_height = functora_egui::text::intrinsic_box_height(&galley, 24.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("resolve(ui, None): {wrap:?}")).show(ui2);
            _ = Typography::small(format!("natural_width: {natural_width:.1}")).show(ui2);
            _ = Typography::small(format!(
                "box_height: {box_height:.1}, intrinsic_box_height: {intrinsic_box_height:.1}"
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "Wrap variants: {:?}, {:?}, {:?}, {:?}",
                functora_egui::text::Wrap::Word,
                functora_egui::text::Wrap::Any,
                functora_egui::text::Wrap::Nowrap,
                functora_egui::text::Wrap::Truncate
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "paint: shared painting helpers");
        let theme: ShadcnTheme = ui.ctx().shadcn_theme();
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "corner_radius(6.0): {:?}",
                functora_egui::paint::corner_radius(6.0)
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "interpolate_color(black, white, 0.5): {:?}",
                functora_egui::paint::interpolate_color::interpolate_color(
                    egui::Color32::BLACK,
                    egui::Color32::WHITE,
                    0.5
                )
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "paint_arc / paint_dashed_rect / paint_focus_ring use theme.primary: {:?}",
                theme.primary
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "tokens: design tokens");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "AlertVariant::Destructive.semantic(): {:?}",
                functora_egui::tokens::alert_variant::AlertVariant::Destructive.semantic()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "BadgeVariant::Success.semantic(): {:?}",
                functora_egui::tokens::badge_variant::BadgeVariant::Success.semantic()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ToastVariant::Error.semantic(): {:?}",
                functora_egui::tokens::toast_variant::ToastVariant::Error.semantic()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ComponentSize::Default.metrics(): {:?}",
                functora_egui::tokens::component_size::ComponentSize::Default.metrics()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ComponentSize::Default.metrics_for(mobile): {:?}",
                functora_egui::tokens::component_size::ComponentSize::Default
                    .metrics_for(&Spacing::for_breakpoint(functora_egui::Breakpoint::Mobile))
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ComponentSize::Default.metrics_for(desktop): {:?}",
                functora_egui::tokens::component_size::ComponentSize::Default
                    .metrics_for(&Spacing::for_breakpoint(functora_egui::Breakpoint::Desktop))
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "TypographyVariant::H1.font_size(): {}",
                functora_egui::tokens::typography_variant::TypographyVariant::H1.font_size()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "TypographyVariant::H1.foreground(theme): {:?}",
                functora_egui::tokens::typography_variant::TypographyVariant::H1.foreground(&theme)
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "TypographyVariant::Muted.foreground(theme): {:?}",
                functora_egui::tokens::typography_variant::TypographyVariant::Muted
                    .foreground(&theme)
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "SemanticColor::Destructive.color(theme): {:?}",
                functora_egui::tokens::semantic_color::SemanticColor::Destructive.color(&theme)
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "style resolution: the exact colors a widget paints");
        let spacing = ui.responsive_spacing();
        let idle = functora_egui::widgets::interaction::Interaction::IDLE;
        let button_style =
            functora_egui::widgets::button::button_variant_style::resolve_button_style(
                &theme,
                &spacing,
                functora_egui::ButtonVariant::Default,
                functora_egui::ComponentSize::Default,
                &idle,
            );
        let badge_style = functora_egui::widgets::badge::badge_variant_style::resolve_badge_style(
            &theme,
            functora_egui::BadgeVariant::Default,
        );
        let alert_style = functora_egui::widgets::alert::alert_variant_style::resolve_alert_style(
            &theme,
            functora_egui::AlertVariant::Success,
        );
        let checkbox_style =
            functora_egui::widgets::checkbox::checkbox_style::resolve_checkbox_style(
                &theme, true, &idle,
            );
        let input_style =
            functora_egui::widgets::input::input_style::resolve_input_style(&theme, true);
        let radio_style =
            functora_egui::widgets::radio::radio_style::resolve_radio_style(&theme, true, &idle);
        let select_style =
            functora_egui::widgets::select::select_style::resolve_select_style(&theme);
        let slider_style =
            functora_egui::widgets::slider::slider_style::resolve_slider_style(&theme);
        let switch_style = functora_egui::widgets::switch::switch_style::resolve_switch_style(
            &theme, true, 1.0, false,
        );
        let toggle_style = functora_egui::widgets::toggle::toggle_style::resolve_toggle_style(
            &theme,
            functora_egui::ToggleVariant::Default,
            false,
            &idle,
        );
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "ResolvedButtonStyle: bg {:?}, fg {:?}, height {}, font_size {}, underline {}",
                button_style.bg,
                button_style.fg,
                button_style.height,
                button_style.font_size,
                button_style.underline
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedBadgeStyle: bg {:?}, fg {:?}, border {:?}",
                badge_style.bg, badge_style.fg, badge_style.border
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedAlertStyle: bg {:?}, fg {:?}, border {:?}",
                alert_style.bg, alert_style.fg, alert_style.border
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedCheckboxStyle: box_bg {:?}, check_color {:?}, text_color {:?}",
                checkbox_style.box_bg, checkbox_style.check_color, checkbox_style.text_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedInputStyle: bg {:?}, border_color {:?}, text_color {:?}",
                input_style.bg, input_style.border_color, input_style.text_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedRadioStyle: circle_border {:?}, dot_color {:?}, text_color {:?}",
                radio_style.circle_border, radio_style.dot_color, radio_style.text_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedSelectStyle: trigger_bg {:?}, popover_bg {:?}, item_hover_bg {:?}",
                select_style.trigger_bg, select_style.popover_bg, select_style.item_hover_bg
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedSliderStyle: track_color {:?}, fill_color {:?}, handle_fill {:?}",
                slider_style.track_color, slider_style.fill_color, slider_style.handle_fill
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedSwitchStyle: track_color {:?}, thumb_color {:?}",
                switch_style.track_color, switch_style.thumb_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ResolvedToggleStyle: bg {:?}, fg {:?}, border {:?}",
                toggle_style.bg, toggle_style.fg, toggle_style.border
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "interaction: pointer and enablement state");
        let idle_state = functora_egui::widgets::interaction::Interaction::IDLE;
        let measuring = functora_egui::widgets::interaction::Interaction::measuring(false);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "Interaction::IDLE: hovered {}, pressed {}, disabled {}",
                idle_state.hovered, idle_state.pressed, idle_state.disabled
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "Interaction::measuring(false): hovered {}, pressed {}, disabled {}",
                measuring.hovered, measuring.pressed, measuring.disabled
            ))
            .show(ui2);
            let response = ui2.add(egui::Label::new("hover me"));
            let live = functora_egui::widgets::interaction::Interaction::of(&response, true);
            _ = Typography::small(format!(
                "Interaction::of(response, true): hovered {}, pressed {}, disabled {}",
                live.hovered, live.pressed, live.disabled
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "theme: light/dark detection");
        let ctx = ui.ctx().clone();
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "default_theme(ctx): {:?}, detect_system_theme(ctx): {:?}, detect_system_theme_wasm(): {:?}",
                functora_egui::default_theme(&ctx),
                functora_egui::detect_system_theme(&ctx),
                functora_egui::theme_extra::detect_system_theme_wasm()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "current_theme(ctx): {:?}, Theme::Dark.as_str(): {:?}, Theme::Light.next(): {:?}",
                functora_egui::current_theme(&ctx),
                functora_egui::theme_extra::Theme::Dark.as_str(),
                functora_egui::theme_extra::Theme::Light.next()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ShadcnTheme: primary {:?}, radius {}, foreground {:?}",
                theme.primary, theme.radius, theme.foreground
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "utils: compatibility facade");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "utils::f32_to_u8_clamped(12.6) = {}",
                functora_egui::utils::f32_to_u8_clamped(12.6)
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "utils::pixel_area_len(3, 3) = {}",
                functora_egui::utils::pixel_area_len(3, 3)
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "config / icon_export / build");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "config::capitalize_words(\"functora egui demo\"): {}",
                functora_egui::config::shared::capitalize_words("functora egui demo")
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "config::pkg_name(config::parse_toml(\"Cargo.toml\")): {:?}",
                packaging.pkg_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "config::lib_name_from(...): {:?}",
                packaging.lib_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "config::app_name_from_manifest(\"Cargo.toml\"): {:?}",
                packaging.app_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "config::derive_pkg_js(\"Cargo.toml\"): {:?}",
                packaging.pkg_js
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "config::derive_lib_name(\"Cargo.toml\"): {:?}",
                packaging.derived_lib_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "icon_export::IconSet::Launcher = {}, IconSet::Desktop = {}",
                functora_egui::icon_export::IconSet::Launcher,
                functora_egui::icon_export::IconSet::Desktop
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "icon_export::IconsError: {:?}",
                std::mem::discriminant(&functora_egui::icon_export::IconsError::CreateDir {
                    set: functora_egui::icon_export::IconSet::Launcher,
                    dir: std::path::PathBuf::from("icons"),
                    source: std::io::Error::other("demo"),
                })
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "build::BuildConfigs from build.rs: DEMO_WEB_TITLE = {}",
                env!("DEMO_WEB_TITLE")
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "build::BuildConfigs type: {}",
                std::any::type_name::<functora_egui::build::BuildConfigs>()
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "packaging: per-platform build configuration");
        let web_config = &packaging.web;
        let android_config = &packaging.android;
        let desktop_config = &packaging.desktop;
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "WebConfig: title {:?}, short_name {:?}, theme_color {:?}, pkg_js {:?}, vsn {:?}",
                web_config.title,
                web_config.short_name,
                web_config.theme_color,
                web_config.pkg_js,
                web_config.vsn
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "web::derive_title(\"Cargo.toml\"): {:?}, web::derive_theme_color(\"Cargo.toml\"): {:?}",
                packaging.web_title, packaging.web_theme_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "AndroidConfig: namespace {:?}, application_id {:?}, activity_fqn {:?}, version_code {}",
                android_config.namespace,
                android_config.application_id,
                android_config.activity_fqn,
                android_config.version_code
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "DesktopConfig: app_id {:?}, title {:?}, exec_name {:?}, icon_name {:?}",
                desktop_config.app_id,
                desktop_config.title,
                desktop_config.exec_name,
                desktop_config.icon_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "desktop::release_date(): {:?}, iso_date_from_days_since_epoch(0): {:?}",
                packaging.release_date,
                functora_egui::desktop::config::iso_date_from_days_since_epoch(0)
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "desktop::release_date_from(None): {:?}, release_date_from(Some(0)): {:?}",
                functora_egui::desktop::config::release_date_from(None),
                functora_egui::desktop::config::release_date_from(Some(0))
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "manifest parsed on demand: {:?}",
                packaging.manifest_path
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "packaging templates");
        let index_html = functora_egui::web::templates::IndexHtml {
            title: &web_config.title,
            theme_color: &web_config.theme_color,
            pkg_js: &web_config.pkg_js,
            vsn: &web_config.vsn,
        };
        let manifest_json = functora_egui::web::templates::ManifestJson {
            title: &web_config.title,
            short_name: &web_config.short_name,
            theme_color: &web_config.theme_color,
        };
        let settings_gradle = functora_egui::android::templates::SettingsGradle {
            app_name: &android_config.app_name,
        };
        let app_build_gradle = functora_egui::android::templates::AppBuildGradle {
            namespace: &android_config.namespace,
            application_id: &android_config.application_id,
            version_code: android_config.version_code,
            version_name: &android_config.version_name,
        };
        let manifest_xml = functora_egui::android::templates::ManifestXml {
            label: &android_config.label,
            activity_fqn: &android_config.activity_fqn,
            lib_name: &android_config.lib_name,
            host: &android_config.host,
            path_prefix: &android_config.path_prefix,
            extra_intent_filters: &android_config.extra_intent_filters,
            camera: android_config.camera,
            internet: android_config.internet,
        };
        let main_activity = functora_egui::android::templates::MainActivity {
            package: &android_config.namespace,
            lib_name: &android_config.lib_name,
        };
        let linux_desktop = functora_egui::desktop::templates::LinuxDesktop {
            title: &desktop_config.title,
            comment: &desktop_config.comment,
            exec_name: &desktop_config.exec_name,
            icon_name: &desktop_config.icon_name,
            categories: &desktop_config.categories,
            mime_types: &desktop_config.mime_types,
            schemes: &desktop_config.schemes,
        };
        let metainfo = functora_egui::desktop::templates::Metainfo {
            app_id: &desktop_config.app_id,
            title: &desktop_config.title,
            comment: &desktop_config.comment,
            description: &desktop_config.description,
            version: &desktop_config.version,
            date: &packaging.release_date,
            homepage: &desktop_config.homepage,
            developer: &desktop_config.developer,
            developer_id: &desktop_config.developer_id,
        };
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "IndexHtml: title {:?}, pkg_js {:?}, vsn {:?}",
                index_html.title, index_html.pkg_js, index_html.vsn
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ManifestJson: title {:?}, short_name {:?}, theme_color {:?}",
                manifest_json.title, manifest_json.short_name, manifest_json.theme_color
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "SettingsGradle: app_name {:?}",
                settings_gradle.app_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "AppBuildGradle: namespace {:?}, application_id {:?}, version_code {}, version_name {:?}",
                app_build_gradle.namespace,
                app_build_gradle.application_id,
                app_build_gradle.version_code,
                app_build_gradle.version_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "ManifestXml: label {:?}, activity_fqn {:?}, lib_name {:?}, host {:?}, path_prefix {:?}, camera {}, internet {}",
                manifest_xml.label,
                manifest_xml.activity_fqn,
                manifest_xml.lib_name,
                manifest_xml.host,
                manifest_xml.path_prefix,
                manifest_xml.camera,
                manifest_xml.internet
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "MainActivity: package {:?}, lib_name {:?}",
                main_activity.package, main_activity.lib_name
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "LinuxDesktop: title {:?}, exec_name {:?}, icon_name {:?}, categories {:?}",
                linux_desktop.title,
                linux_desktop.exec_name,
                linux_desktop.icon_name,
                linux_desktop.categories
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "Metainfo: app_id {:?}, version {:?}, date {:?}, homepage {:?}",
                metainfo.app_id, metainfo.version, metainfo.date, metainfo.homepage
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "Static templates: {}, {}, {}, {}",
                std::any::type_name::<functora_egui::android::templates::RootBuildGradle>(),
                std::any::type_name::<functora_egui::android::templates::GradleProperties>(),
                std::any::type_name::<functora_egui::android::templates::Styles>(),
                std::any::type_name::<functora_egui::android::templates::Waker>()
            ))
            .show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "packaging icons");
        _ = Card::new().show(ui, |ui2| {
            if Button::new("Copy icons from missing dirs")
                .variant(ButtonVariant::Outline)
                .show(ui2)
                .clicked()
            {
                let launcher = functora_egui::android::icons::copy_launcher_icons(
                    std::path::Path::new("/nonexistent-functora-demo-android"),
                    std::path::Path::new("/nonexistent-functora-demo-favicon"),
                );
                _ = Typography::small(format!(
                    "android::icons::copy_launcher_icons(missing dirs): {:?}",
                    launcher.map(|paths| paths.len())
                ))
                .show(ui2);
                let desktop_icons = functora_egui::desktop::icons::copy_desktop_icons(
                    std::path::Path::new("/nonexistent-functora-demo-desktop"),
                    std::path::Path::new("/nonexistent-functora-demo-assets"),
                    &desktop_config.app_id,
                );
                _ = Typography::small(format!(
                    "desktop::icons::copy_desktop_icons(missing dirs): {:?}",
                    desktop_icons.map(|paths| paths.len())
                ))
                .show(ui2);
            }
        });

        #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
        {
            ui.add_space(12.0);
            crate::screens::section(ui, "desktop runner");
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(format!(
                    "desktop::run::launch_args(): {:?}",
                    functora_egui::desktop::run::launch_args()
                ))
                .show(ui2);
                _ = Typography::small(format!(
                    "desktop::run::take_file_args(): {:?}",
                    functora_egui::desktop::run::take_file_args()
                ))
                .show(ui2);
                _ = Typography::small(format!(
                    "desktop::run::icon_from_png(empty): {:?}",
                    functora_egui::desktop::run::icon_from_png(&[])
                ))
                .show(ui2);
                _ = Typography::small(format!(
                    "platform::desktop::storage_get(\"functora-egui-demo\", \"nope\"): {:?}",
                    functora_egui::platform::desktop::storage_get("functora-egui-demo", "nope")
                ))
                .show(ui2);
                if Button::new("Write storage value")
                    .variant(ButtonVariant::Outline)
                    .show(ui2)
                    .clicked()
                {
                    let stored = functora_egui::platform::desktop::storage_set(
                        "functora-egui-demo",
                        "utilities_demo",
                        "hello",
                    );
                    _ = Typography::small(format!(
                        "platform::desktop::storage_set(...): {:?}",
                        stored.is_ok()
                    ))
                    .show(ui2);
                }
                _ = Typography::small(
                    "platform::desktop::open_url opens a URL in the desktop browser.",
                )
                .show(ui2);
            });
        }

        ui.add_space(12.0);
        crate::screens::section(ui, "platform runtime: back handling and startup");
        _ = Card::new().show(ui, |ui2| {
            let runtime_ctx = ui2.ctx().clone();
            _ = Typography::small(format!(
                "android_back::is_back_pressed(ctx): {}, BackOutcome: {:?} / {:?}",
                functora_egui::platform::android_back::is_back_pressed(&runtime_ctx),
                functora_egui::platform::android_back::BackOutcome::ConsumedNav,
                functora_egui::platform::android_back::BackOutcome::ConsumedNoop
            ))
            .show(ui2);
            if Button::new("Store context and wake")
                .variant(ButtonVariant::Outline)
                .show(ui2)
                .clicked()
            {
                functora_egui::platform::android_back::store_context(&runtime_ctx);
                functora_egui::platform::android_back::wake_via_repaint();
            }
            _ = Typography::small(format!(
                "web::startup::is_mobile(ctx): {}",
                functora_egui::web::startup::is_mobile(&runtime_ctx)
            ))
            .show(ui2);
            #[cfg(not(target_arch = "wasm32"))]
            {
                _ = Typography::small(format!(
                    "web::startup::startup_width(ctx): {:.1}, is_mobile_initial_ctx(ctx): {}",
                    functora_egui::web::startup::startup_width(&runtime_ctx),
                    functora_egui::web::startup::is_mobile_initial_ctx(&runtime_ctx)
                ))
                .show(ui2);
            }
            #[cfg(target_arch = "wasm32")]
            {
                _ = Typography::small(
                    "web::startup::startup_width(cc) and is_mobile_initial(cc) take the creation context on web.",
                )
                .show(ui2);
                _ = Typography::small(format!(
                    "platform::web::location_pathname(): {:?}",
                    functora_egui::platform::web::location_pathname()
                ))
                .show(ui2);
                _ = Typography::small(
                    "platform::web::install_paste_image_listener() registers the paste listener.",
                )
                .show(ui2);
            }
            #[cfg(target_os = "android")]
            {
                _ = Typography::small(
                    "On Android: android::ime::store_app(app), android::intent::get_data_string(app), android::hide_soft_input(), android::wake_event_loop(), android::peek_back_pressed(), android::poll_back_pressed(), android::save_to_downloads().",
                )
                .show(ui2);
            }
        });

        ui.add_space(12.0);
        snippet(
            ui,
            "// Utilities: the public helper modules\nuse functora_egui::cast::{cap_to_available, f32_to_u8_clamped, f32_to_usize_clamped, f64_to_f32, f64_to_i32, f64_to_u64_clamped, i32_to_f32, scaled_px, u32_to_f32, u64_to_f64_js, usize_to_f32, usize_to_u32};\nuse functora_egui::frame::{FrameRotation, fit_preview_size, fps_to_interval_ms, nv21_luma, nv21_to_rgba, pixel_area_len, responsive_preview_size, rotate_luma, rotate_rgba, rotated_dimensions};\nuse functora_egui::text::{Wrap, box_height, intrinsic_box_height, layout, natural_width, resolve};\nuse functora_egui::paint::corner_radius;\nuse functora_egui::paint::interpolate_color::interpolate_color;\nuse functora_egui::paint::paint_arc::paint_arc;\nuse functora_egui::paint::paint_dashed_rect::paint_dashed_rect;\nuse functora_egui::paint::paint_focus_ring::paint_focus_ring;\nuse functora_egui::utils as utils_facade;\n\n// Saturating casts never panic\nassert_eq!(f32_to_u8_clamped(300.0), 255);\nassert_eq!(f32_to_usize_clamped(-4.0), 0);\nassert_eq!(cap_to_available(300.0, 200.0), 200.0);\n\n// Camera buffers\nlet rotation = FrameRotation::from_degrees_cw(90);\nlet (w, h) = rotated_dimensions(4, 2, rotation);\nlet (pixels, out_w, out_h) = rotate_luma(&luma, 4, 2, rotation);\nlet (rgba, rw, rh) = rotate_rgba(&rgba, 4, 2, FrameRotation::Cw180);\nlet preview = fit_preview_size(egui::vec2(100.0, 50.0), 4, 2);\nlet interval = fps_to_interval_ms(30.0); // 33 ms\n\n// Text measurement\nlet galley = layout(ui, \"hello\", font_id, color, 120.0, Some(Wrap::Word));\nlet height = box_height(&galley, 24.0);\nlet intrinsic = intrinsic_box_height(&galley, 24.0);\nlet width = natural_width(ctx, \"hello\", font_id);\nlet wrap = resolve(ui, None);\n\n// Shared painting\nlet radius = corner_radius(6.0);\npaint_arc(painter, center, 8.0, 0.0, egui::Stroke::new(2.0, color));\npaint_dashed_rect(painter, rect, 4.0, egui::Stroke::new(1.0, color), 6.0, 4.0);\npaint_focus_ring(painter, rect, 4.0, color);\nlet mixed = interpolate_color(egui::Color32::BLACK, egui::Color32::WHITE, 0.5);\n\n// The facade re-exports the same helpers\nassert_eq!(utils_facade::f32_to_u8_clamped(12.6), 13);\n\n// Design tokens\nuse functora_egui::tokens::{alert_variant::AlertVariant, component_size::ComponentSize, semantic_color::SemanticColor};\n\nlet semantic = AlertVariant::Destructive.semantic();\nlet (height, padding, radius) = ComponentSize::Default.metrics();\nlet color = SemanticColor::Destructive.color(&theme);\n\n// Build-time configuration\nuse functora_egui::config::shared::{capitalize_words, lib_name_from, parse_toml, pkg_name};\nuse functora_egui::icon_export::IconSet;\n\nlet manifest = parse_toml(\"Cargo.toml\");\nlet name = manifest.as_ref().and_then(pkg_name);\nlet lib = manifest.as_ref().and_then(lib_name_from);\nlet title = capitalize_words(\"functora egui demo\");\nlet icons = IconSet::Launcher;\n\n// Manifest-driven build metadata\nuse functora_egui::config::shared::{app_name_from_manifest, derive_lib_name, derive_pkg_js};\n\nlet app_name = app_name_from_manifest(\"Cargo.toml\");\nlet js_bundle = derive_pkg_js(\"Cargo.toml\");\nlet lib_name = derive_lib_name(\"Cargo.toml\");\n\n// Icon export failures are typed\nuse functora_egui::icon_export::IconsError;\n\nlet err = IconsError::CreateDir {\n    set: IconSet::Launcher,\n    dir: std::path::PathBuf::from(\"icons\"),\n    source: std::io::Error::other(\"nope\"),\n};\n\n// Responsive metrics scale with the spacing scale\nuse functora_egui::{Breakpoint, ComponentSize, Spacing, TypographyVariant};\n\nlet mobile = Spacing::for_breakpoint(Breakpoint::Mobile);\nlet (height, padding, font_size) = ComponentSize::Default.metrics_for(&mobile);\nlet foreground = TypographyVariant::H1.foreground(&theme);\n\n// Style resolution returns the exact colors a widget paints\nuse functora_egui::widgets::button::button_variant_style::resolve_button_style;\nuse functora_egui::widgets::interaction::Interaction;\n\nlet style = resolve_button_style(\n    &theme,\n    &spacing,\n    functora_egui::ButtonVariant::Default,\n    ComponentSize::Default,\n    &Interaction::IDLE,\n);\n\n// Interaction describes the pointer state a style is resolved from\nlet idle = Interaction::IDLE;\nlet measuring = Interaction::measuring(false);\n\n// Theme detection\nuse functora_egui::{default_theme, detect_system_theme};\n\nlet system = detect_system_theme(ctx);\nlet fallback = default_theme(ctx);\n\n// Per-platform packaging configuration\nuse functora_egui::android::config::load_android_config;\nuse functora_egui::desktop::config::load_desktop_config;\nuse functora_egui::web::config::load_config;\n\nlet web = load_config(\"Cargo.toml\");\nlet android = load_android_config(\"Cargo.toml\");\nlet desktop = load_desktop_config(\"Cargo.toml\");\n\n// A platform without a clock falls back to the Unix epoch\nuse functora_egui::desktop::config::{release_date, release_date_from};\n\nlet today = release_date();\nlet epoch = release_date_from(None);\n\n// The templates that turn those configs into files\nuse functora_egui::android::templates::{AppBuildGradle, ManifestXml, RootBuildGradle, SettingsGradle};\nuse functora_egui::desktop::templates::{LinuxDesktop, Metainfo};\nuse functora_egui::web::templates::{IndexHtml, ManifestJson};\n\nlet index = IndexHtml {\n    title: &web.title,\n    theme_color: &web.theme_color,\n    pkg_js: &web.pkg_js,\n    vsn: &web.vsn,\n};\n\n// Launcher icons are copied from the favicon directory\nuse functora_egui::android::icons::copy_launcher_icons;\nuse functora_egui::desktop::icons::copy_desktop_icons;\n\nlet copied = copy_launcher_icons(&android_dir, &favicon_dir)?;\n\n// The desktop runner takes the app id, the title and an optional window icon\nuse functora_egui::desktop::run::{ingest_launch_args, run, run_with_icon};\n\ningest_launch_args();\nrun(\"My app\", \"io.functora.my_app\", |cc| Ok(Box::new(MyApp::new(cc))));\nrun_with_icon(\"My app\", \"io.functora.my_app\", icon, |cc| Ok(Box::new(MyApp::new(cc))));\n\n// System back handling: the browser back key and the Android back button\nuse functora_egui::platform::android_back::{BackOutcome, handle_system_back, is_back_pressed};\n\nif is_back_pressed(ctx) {\n    // the app can decide whether to navigate back\n}\n\nif let Some(outcome) = handle_system_back(ctx, can_go_back, || go_back()) {\n    match outcome {\n        BackOutcome::ConsumedNav => { /* the app navigated back */ }\n        BackOutcome::ConsumedNoop => { /* the keyboard was dismissed */ }\n    }\n}\n\n// egui_flex is re-exported for full control over the flex items\nuse functora_egui::egui_flex;\n\nlet item = egui_flex::FlexItem::new().grow(1.0);",
        );
    }
}
