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

#[test]
fn metainfo_embeds_app_id_and_version() {
    let rendered = Metainfo {
        app_id: "io.functora.cryptonote_egui",
        title: "Cryptonote",
        comment: "Encrypted notes",
        version: "0.1.10",
    }
    .render()
    .expect("render metainfo");
    for fragment in [
        "<id>io.functora.cryptonote_egui.desktop</id>",
        "<name>Cryptonote</name>",
        "<summary>Encrypted notes</summary>",
        "<release version=\"0.1.10\" />",
        "<launchable type=\"desktop-id\">io.functora.cryptonote_egui.desktop</launchable>",
    ] {
        assert!(
            rendered.contains(fragment),
            "metainfo must contain {fragment}"
        );
    }
}
