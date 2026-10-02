use functora_egui::snippet;
use functora_egui::{Card, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_package(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Package: FUNCTORA_CORE_DATE/YEAR + Cargo.toml metadata (title, theme_color) via build.rs env.",
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
        snippet(
            ui,
            "// Package: compile-time metadata via env! + functora-core stamps\nuse functora_egui::{FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR};\n\n// From Cargo.toml, resolved at compile time\nlet name = env!(\"CARGO_PKG_NAME\");\nlet version = env!(\"CARGO_PKG_VERSION\");\n\n// build.rs resolves these from Cargo.toml [package.metadata.functora-egui-web]\nlet title = env!(\"DEMO_WEB_TITLE\");\nlet theme_color = env!(\"DEMO_WEB_THEME_COLOR\");\n\n// Library build stamps\nlet date = FUNCTORA_CORE_DATE;\nlet year = FUNCTORA_CORE_YEAR;\n\neprintln!(\"{name} {version} - {title} ({date}, {year})\");",
        );
    }

    /// White-label branding for this demo crate, the same way an app
    /// declares `AppAttrs` for URLs, store links and footer text.
    pub const DEMO_ATTRS: functora_egui::white_label::AppAttrs =
        functora_egui::white_label::AppAttrs {
            app: "functora-egui-demo",
            vsn: env!("CARGO_PKG_VERSION"),
            org: "functora",
            src: Some("rust"),
            dst: "apps",
            description: env!("CARGO_PKG_DESCRIPTION"),
        };
}
