use functora_egui::snippet;
use functora_egui::{Card, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_white_label(&mut self, ui: &mut egui::Ui) {
        use functora_egui::i18n::{I18N, Language};
        use functora_egui::white_label::{
            BTC_ADDRESS, CryptoBlock, DEFAULT_SIDEBAR_ICONS, FUNCTORA_EMAIL, FUNCTORA_ORG,
            ManifestIcon, QUIT_AVAILABLE, XMR_ADDRESS, default_about_text, default_donate_blocks,
            default_donate_greeting, default_donate_intro, default_footer, default_license_text,
            default_privacy_text, default_sidebar_labels, license_text, manifest_json,
            privacy_text,
        };

        _ = Typography::muted(
            "WhiteLabel: functora_core::white_label - AppAttrs branding URLs, donate blocks, WhiteLabelContent defaults, manifest, footer, sidebar labels and the default license/privacy/about/donate screens.",
        )
        .show(ui);
        ui.add_space(12.0);
        let attrs = Self::DEMO_ATTRS;
        let content =
            functora_egui::white_label::WhiteLabelContent::<functora_egui::messages::Msg>::default(
            );
        let custom_branding = content.license_text.is_some() || content.privacy_text.is_some();
        let donate_labels = content
            .donate_blocks
            .iter()
            .map(|block| block.label.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "app: {} v{} ({})",
                attrs.app, attrs.vsn, attrs.description
            ))
            .show(ui2);
            _ = Typography::small(format!("app_url: {}", attrs.app_url())).show(ui2);
            _ = Typography::small(format!("source_url: {}", attrs.source_url())).show(ui2);
            _ = Typography::small(format!("custom branding: {custom_branding}")).show(ui2);
            _ = Typography::small(format!("donate_blocks: {donate_labels}")).show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Derived branding URLs");
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for (label, value) in [
                    ("app_name", attrs.app_name()),
                    ("cache_name", attrs.cache_name()),
                    ("pages_url", attrs.pages_url()),
                    ("author_url", attrs.author_url()),
                    ("app_url", attrs.app_url()),
                    ("app_version_url", attrs.app_version_url()),
                    ("origin", attrs.origin()),
                    ("source_url", attrs.source_url()),
                    ("apk_url", attrs.apk_url()),
                    ("google_play_url", attrs.google_play_url()),
                    ("beta_url", attrs.beta_url()),
                    ("share_anchor_id", attrs.share_anchor_id()),
                    (
                        "manifest_uri",
                        format!("{:?}", attrs.manifest_uri("/icon-192.png", "/icon-512.png")),
                    ),
                ] {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{label}: {value}")).show(ui3);
                    });
                }
            });
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Author mention split");
        let greeting = default_donate_greeting(&attrs, Language::Eng);
        let mention = attrs.split_author_mention(&greeting);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "split_author_mention: {mention:?} (org = {FUNCTORA_ORG}, email = {FUNCTORA_EMAIL})"
            ))
            .show(ui2);
            _ = Typography::small(format!("BTC: {BTC_ADDRESS}\nXMR: {XMR_ADDRESS}")).show(ui2);
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Default legal and donate texts");
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for (label, value) in [
                    (
                        "license_text(org)",
                        license_text(FUNCTORA_ORG, Language::Eng).len(),
                    ),
                    (
                        "default_license_text",
                        default_license_text(&attrs, Language::Eng).len(),
                    ),
                    (
                        "privacy_text(provider, email)",
                        privacy_text(FUNCTORA_ORG, FUNCTORA_EMAIL, Language::Eng).len(),
                    ),
                    (
                        "default_privacy_text",
                        default_privacy_text(&attrs, Language::Eng).len(),
                    ),
                    (
                        "default_about_text",
                        default_about_text(&attrs, Language::Eng).len(),
                    ),
                    (
                        "default_donate_greeting",
                        default_donate_greeting(&attrs, Language::Eng).len(),
                    ),
                    (
                        "default_donate_intro",
                        default_donate_intro(&attrs, Language::Eng).len(),
                    ),
                ] {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{label}: {value} chars")).show(ui3);
                    });
                }
            });
        });

        ui.add_space(12.0);
        crate::screens::section(ui, "Manifest, sidebar labels and footer");
        let icons = [
            ManifestIcon {
                src: "/icon-192.png".to_owned(),
                sizes: "192x192",
                r#type: "image/png",
                purpose: "any",
            },
            ManifestIcon {
                src: "/icon-512.png".to_owned(),
                sizes: "512x512",
                r#type: "image/png",
                purpose: "any",
            },
        ];
        let manifest = manifest_json(
            attrs.app,
            attrs.vsn,
            attrs.description,
            &attrs.app_url(),
            &attrs.app_url(),
            &icons,
        );
        let labels = default_sidebar_labels(Language::Eng);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("manifest_json: {} chars", manifest.len())).show(ui2);
            _ = Typography::small(format!("sidebar labels: {labels:?}")).show(ui2);
            _ = Typography::small(format!(
                "sidebar icons: {:?}",
                DEFAULT_SIDEBAR_ICONS.map(|icon| icon.name())
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "donate blocks: {:?}",
                default_donate_blocks()
                    .iter()
                    .map(|block| block.label.as_str())
                    .collect::<Vec<_>>()
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "CryptoBlock {{ label, address }}: {:?}",
                default_donate_blocks()
                    .first()
                    .map(|block: &CryptoBlock| block.label.as_str())
            ))
            .show(ui2);
        });
        ui.add_space(4.0);
        _ = Typography::small("default_footer (click a link to get its action id):").show(ui);
        if let Some(action) = default_footer(ui, &attrs, Language::Eng) {
            _ = Typography::small(format!("footer action: {action}")).show(ui);
        }

        ui.add_space(12.0);
        crate::screens::section(ui, "Desktop-only quit entry");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("QUIT_AVAILABLE: {QUIT_AVAILABLE}")).show(ui2);
            for lang in functora_egui::i18n::SUPPORTED_LANGUAGES {
                _ = Typography::small(format!(
                    "quit label {lang}: {}",
                    functora_egui::messages::Msg::Quit.render(*lang)
                ))
                .show(ui2);
            }
        });
        _ = Typography::muted(
            "default_sidebar renders the navigation entries, then Reset, then - where QUIT_AVAILABLE is true - Quit as the last entry. The picked entry comes back as a SidebarAction and SidebarAction::Quit is closed with platform::request_quit, which saves the app and leaves the event loop.",
        )
        .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Default screens");
        let mut requested = None;
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(functora_egui::Button::new("License")).inner.clicked() {
                requested = Some("license");
            }
            if f.add(functora_egui::Button::new("Privacy")).inner.clicked() {
                requested = Some("privacy");
            }
            if f.add(functora_egui::Button::new("About")).inner.clicked() {
                requested = Some("about");
            }
            if f.add(functora_egui::Button::new("Donate")).inner.clicked() {
                requested = Some("donate");
            }
        });
        match requested {
            Some("license") => functora_egui::white_label::screen_license(
                ui,
                &mut self.platform.md_cache,
                &attrs,
                Language::Eng,
            ),
            Some("privacy") => functora_egui::white_label::screen_privacy(
                ui,
                &mut self.platform.md_cache,
                &attrs,
                Language::Eng,
            ),
            Some("about") => functora_egui::white_label::screen_about(
                ui,
                &mut self.platform.md_cache,
                &attrs,
                Language::Eng,
            ),
            Some("donate") => {
                let now = ui.ctx().input(|i| i.time);
                functora_egui::white_label::screen_donate(
                    ui,
                    &attrs,
                    Language::Eng,
                    &content,
                    now,
                    &mut self.toast,
                );
            }
            Some(_) => {}
            None => _ = Typography::small("Pick a screen to render it.").show(ui),
        }

        ui.add_space(12.0);
        crate::screens::section(ui, "WhiteLabelApp skeleton");
        let ctx = ui.ctx().clone();
        let mut app = functora_egui::white_label::WhiteLabelApp::<crate::route::AppRoute, ()>::new(
            &ctx,
            attrs,
            (),
        )
        .with_nav_entries([(
            crate::route::AppRoute::Overview,
            functora_egui::LucideIcon::House,
        )])
        .with_sidebar_labels(labels);
        app.set_language(Language::Eng);
        app.content_mut().donate_greeting = Some(functora_egui::messages::Msg::DonateGreeting);
        let route = app.router().current().clone();
        _ = app.toast_mut();
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "WhiteLabelApp lang: {}, attrs.app: {}, content.greeting: {:?}",
                app.lang(),
                app.attrs().app,
                app.content().donate_greeting
            ))
            .show(ui2);
            _ = Typography::small(format!("route: {route}")).show(ui2);
        });

        ui.add_space(12.0);
        snippet(
            ui,
            "// WhiteLabel: per-app branding derived from AppAttrs\nuse functora_egui::white_label::{AppAttrs, WhiteLabelContent};\n\nconst ATTRS: AppAttrs = AppAttrs {\n    app: \"functora-egui-demo\",\n    vsn: env!(\"CARGO_PKG_VERSION\"),\n    org: \"functora\",\n    src: Some(\"rust\"),\n    dst: \"apps\",\n    description: env!(\"CARGO_PKG_DESCRIPTION\"),\n};\n\n// Derived branding URLs\nlet app_url = ATTRS.app_url();        // https://functora.github.io/apps/functora-egui-demo\nlet source_url = ATTRS.source_url();  // repo tree link\n\n// Default content (license / privacy / donate blocks)\nlet content = WhiteLabelContent::<functora_egui::messages::Msg>::default();\nlet donate = content.donate_blocks.len();\n\n// Every derived URL and identifier\nlet urls = [\n    ATTRS.app_name(),\n    ATTRS.cache_name(),\n    ATTRS.pages_url(),\n    ATTRS.author_url(),\n    ATTRS.app_url(),\n    ATTRS.app_version_url(),\n    ATTRS.origin(),\n    ATTRS.source_url(),\n    ATTRS.apk_url(),\n    ATTRS.google_play_url(),\n    ATTRS.beta_url(),\n    ATTRS.share_anchor_id(),\n];\n\n// Link the first mention of the author organization\nuse functora_egui::white_label::AuthorMention;\n\nif let Some(mention): Option<AuthorMention<'_>> = ATTRS.split_author_mention(\"Built by functora and friends\") {\n    eprintln!(\"{} {} {}\", mention.before, mention.name, mention.after);\n}\n\n// Default legal texts, manifest, sidebar labels and footer\nuse functora_egui::white_label::{ManifestIcon, default_about_text, default_donate_greeting, default_donate_intro, default_footer, default_license_text, default_privacy_text, default_sidebar_labels, manifest_json, DEFAULT_SIDEBAR_ICONS};\n\nlet manifest = manifest_json(ATTRS.app, ATTRS.vsn, ATTRS.description, &ATTRS.app_url(), &ATTRS.app_url(), &[ManifestIcon { src: \"/icon-192.png\".to_owned(), sizes: \"192x192\", r#type: \"image/png\", purpose: \"any\" }]);\nlet labels = default_sidebar_labels(functora_egui::i18n::Language::Eng);\nlet action = default_footer(ui, &ATTRS, functora_egui::i18n::Language::Eng); // Some(\"license\")\n\n// The default screens render the legal documents and the donate QR codes\nuse functora_egui::white_label::{screen_about, screen_donate, screen_license, screen_privacy};\n\nscreen_license(ui, &mut cache, &ATTRS, functora_egui::i18n::Language::Eng);\nscreen_privacy(ui, &mut cache, &ATTRS, functora_egui::i18n::Language::Eng);\nscreen_about(ui, &mut cache, &ATTRS, functora_egui::i18n::Language::Eng);\nscreen_donate(ui, &ATTRS, functora_egui::i18n::Language::Eng, &content, now, &mut toast);\n\n// The whole shell: sidebar, footer, routing, language and theme\nuse functora_egui::white_label::WhiteLabelApp;\n\nlet mut app = WhiteLabelApp::<MyRoute, MyState>::new(ctx, ATTRS, MyState::default())\n    .with_nav_entries([(MyRoute::Home, functora_egui::LucideIcon::House)])\n    .with_sidebar_labels(labels);\napp.set_language(functora_egui::i18n::Language::Eng);\napp.navigate(MyRoute::Home);\n\n// The default sidebar ends with a desktop-only quit entry\nuse functora_egui::white_label::{QUIT_AVAILABLE, SidebarAction, default_sidebar};\n\nif QUIT_AVAILABLE {\n    // The picked entry comes back as an action the app handles\n    match default_sidebar(ui, &entries, \"Reset\", &selected, functora_egui::i18n::Language::Eng) {\n        Some(SidebarAction::Navigate(route)) => app.navigate(route),\n        Some(SidebarAction::Reset) => needs_reset = true,\n        // Quit closes the window gracefully: the app saves and exits\n        Some(SidebarAction::Quit) => functora_egui::platform::request_quit(ctx),\n        None => {}\n    }\n}",
        );
    }
}
