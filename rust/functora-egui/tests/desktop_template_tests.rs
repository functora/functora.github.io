#![cfg(any(feature = "build", feature = "desktop"))]
#![allow(clippy::unwrap_used, clippy::expect_used)]
use askama::Template;
use functora_egui::desktop::templates::{LinuxDesktop, Metainfo};

fn entry(mime: &str) -> String {
    LinuxDesktop {
        title: "Cryptonote",
        comment: "Encrypted notes",
        exec_name: "cryptonote-egui",
        icon_name: "io.functora.cryptonote_egui",
        categories: "Utility;Security;",
        mime_types: mime,
        schemes: "",
    }
    .render()
    .expect("render desktop entry")
}

#[test]
fn entry_embeds_identity_and_exec() {
    let rendered = entry("application/x-cryptonote;");
    for fragment in [
        "Name=Cryptonote",
        "Comment=Encrypted notes",
        "Exec=cryptonote-egui %F",
        "Icon=io.functora.cryptonote_egui",
        "Categories=Utility;Security;",
        "MimeType=application/x-cryptonote;",
        "Type=Application",
        "Terminal=false",
    ] {
        assert!(
            rendered.contains(fragment),
            "desktop entry must contain {fragment}"
        );
    }
}

#[test]
fn entry_omits_mime_line_when_empty() {
    let rendered = entry("");
    assert!(!rendered.contains("MimeType="));
}

fn metainfo(developer: &str) -> String {
    Metainfo {
        app_id: "io.functora.cryptonote_egui",
        title: "Cryptonote",
        comment: "Encrypted notes",
        description: "Encrypted offline notes with Markdown support and QR sharing.",
        version: "0.1.10",
        date: "2026-10-08",
        homepage: "https://functora.github.io/cryptonote",
        developer,
        developer_id: "io.functora.cryptonote_egui",
    }
    .render()
    .expect("render metainfo")
}

#[test]
fn metainfo_embeds_app_id_and_version() {
    let rendered = metainfo("Functora");
    for fragment in [
        "<id>io.functora.cryptonote_egui.desktop</id>",
        "<name>Cryptonote</name>",
        "<summary>Encrypted notes</summary>",
        "<release version=\"0.1.10\" date=\"2026-10-08\" />",
        "<launchable type=\"desktop-id\">io.functora.cryptonote_egui.desktop</launchable>",
        "<url type=\"homepage\">https://functora.github.io/cryptonote</url>",
        "<developer id=\"io.functora.cryptonote_egui\"><name>Functora</name></developer>",
        "<content_rating type=\"oars-1.1\">",
        "<p>Encrypted offline notes with Markdown support and QR sharing.</p>",
    ] {
        assert!(
            rendered.contains(fragment),
            "metainfo must contain {fragment}"
        );
    }
}

#[test]
fn metainfo_omits_developer_when_empty() {
    let rendered = metainfo("");
    assert!(!rendered.contains("<developer>"));
}

#[test]
fn metainfo_description_has_no_links() {
    let rendered = metainfo("Functora");
    assert!(!rendered.contains("<a "));
}
