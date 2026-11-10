use functora_egui::snippet;
use functora_egui::{Button, Card, Flex, Input, ToastVariant, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub fn demo_package(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Package: sealed archives (package_async / extract_package_async + read_metadata) and compile-time metadata (FUNCTORA_CORE_DATE/YEAR + Cargo.toml via build.rs env).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "FUNCTORA_CORE_DATE: {}",
                functora_egui::FUNCTORA_CORE_DATE
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "FUNCTORA_CORE_YEAR: {}",
                functora_egui::FUNCTORA_CORE_YEAR
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "crate: {} v{}",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_VERSION")
            ))
            .show(ui2);
            _ = Typography::small(format!("title: {}", env!("DEMO_WEB_TITLE"))).show(ui2);
            _ = Typography::small(format!("theme_color: {}", env!("DEMO_WEB_THEME_COLOR")))
                .show(ui2);
        });
        ui.add_space(12.0);
        crate::screens::section(ui, "Sealed archive (zip + optional encryption)");
        _ = ui.add(
            Input::new(&mut self.platform.package_password)
                .placeholder("archive password")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            let busy = self.platform.package_rx.is_some();
            if f.add(
                Button::new(if busy {
                    "Working..."
                } else {
                    "Pack picked files"
                })
                .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let files = self.platform.picked.clone();
                let password = self.platform.package_password.clone();
                if files.is_empty() {
                    self.toast.add(
                        "No files picked (go to Files)",
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    );
                } else {
                    self.platform.package_rx = Some(spawn_async(async move {
                        Self::package_roundtrip(files, &password).await
                    }));
                }
            }
        });
        ui.add_space(8.0);
        _ = Typography::small(format!(
            "METADATA_ENTRY: {}, PAYLOAD_ENTRY: {}",
            functora_egui::package::METADATA_ENTRY,
            functora_egui::package::PAYLOAD_ENTRY
        ))
        .show(ui);
        _ = Typography::small(format!(
            "aad(prefix, ChaCha20Poly1305, Argon2id): {:?}",
            functora_egui::package::aad(
                b"functora-egui-demo",
                functora_egui::crypto::CipherType::ChaCha20Poly1305,
                functora_egui::crypto::Kdf::Argon2id
            )
        ))
        .show(ui);
        if !self.platform.package_output.is_empty() {
            ui.add_space(8.0);
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(&self.platform.package_output).show(ui2);
            });
        }
        ui.add_space(12.0);
        snippet(
            ui,
            "// Package: compile-time metadata via env! + functora-core stamps\nuse functora_egui::{FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR};\n\n// From Cargo.toml, resolved at compile time\nlet name = env!(\"CARGO_PKG_NAME\");\nlet version = env!(\"CARGO_PKG_VERSION\");\n\n// build.rs resolves these from Cargo.toml [package.metadata.functora-egui-web]\nlet title = env!(\"DEMO_WEB_TITLE\");\nlet theme_color = env!(\"DEMO_WEB_THEME_COLOR\");\n\n// Library build stamps\nlet date = FUNCTORA_CORE_DATE;\nlet year = FUNCTORA_CORE_YEAR;\n\neprintln!(\"{name} {version} - {title} ({date}, {year})\");\n\n// Sealed archives: zip the entries, optionally encrypt the payload\nuse functora_egui::package::{ArchiveMetadata, ArchiveSource, PackageStages, package_async, extract_package_async, package_report, extract_package_report, read_metadata, aad, METADATA_ENTRY, PAYLOAD_ENTRY};\nuse functora_egui::crypto::{CipherType, Kdf};\nuse std::sync::Arc;\nuse std::sync::atomic::AtomicBool;\n\nlet stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);\nlet token = Arc::new(AtomicBool::new(false));\nlet archive = package_async(entries, &password, Some(CipherType::ChaCha20Poly1305), b\"functora-egui-demo\", stages, |_| {}, token.clone()).await?;\n\n// The archive carries metadata.json + payload.cpt\nlet meta: ArchiveMetadata = read_metadata(&ArchiveSource::Bytes(archive.clone()))?;\nassert!(meta.cipher.is_some());\n\n// Unseal it again\nlet plain = extract_package_async(ArchiveSource::Bytes(archive), &password, b\"functora-egui-demo\", stages, |_| {}, token).await?;\n\n// The lower-level report API drives the progress reporter directly\nlet mut report = |job: Job<Stage>| eprintln!(\"{job:?}\");\nlet archive = package_report(entries, &password, Some(CipherType::ChaCha20Poly1305), b\"functora-egui-demo\", stages, &mut report, Some(&token)).await?;\nlet plain = extract_package_report(ArchiveSource::Bytes(archive), &password, b\"functora-egui-demo\", stages, &mut report, Some(&token)).await?;\n\n// The AAD binds the archive prefix to the cipher and KDF\nlet aad = aad(b\"functora-egui-demo\", CipherType::ChaCha20Poly1305, Kdf::Argon2id);",
        );
    }

    /// White-label branding for this demo crate, the same way an app
    /// declares `AppAttrs` for URLs, store links and footer text.
    pub const DEMO_ATTRS: functora_egui::white_label::AppAttrs =
        functora_egui::white_label::functora_attrs(
            "functora-egui-demo",
            env!("CARGO_PKG_VERSION"),
            env!("CARGO_PKG_DESCRIPTION"),
        );
}
