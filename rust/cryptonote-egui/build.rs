#![allow(clippy::unwrap_used, clippy::expect_used)]
fn main() {
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=../egui-shadcn/assets/web/egui.js");
    println!("cargo:rerun-if-changed=../egui-shadcn/templates/web/index.html");
    println!("cargo:rerun-if-changed=../egui-shadcn/templates/web/manifest.json");
    if std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default() != "wasm32" {
        return;
    }
    let manifest_content = std::fs::read_to_string("Cargo.toml").unwrap();
    let manifest: toml::Value = manifest_content.parse().unwrap();
    let package = &manifest["package"];
    let vsn = package["version"].as_str().unwrap_or("0.0.0");
    let pkg_name = package["name"].as_str().unwrap_or("app");
    let lib_name = manifest
        .get("lib")
        .and_then(|lib| lib.get("name"))
        .and_then(|n| n.as_str())
        .map_or_else(
            || format!("{}.js", pkg_name.replace('-', "_")),
            |n| format!("{n}.js"),
        );
    let title = package
        .get("metadata")
        .and_then(|m| m.get("egui-web"))
        .and_then(|w| w.get("title"))
        .and_then(|t| t.as_str())
        .map_or_else(
            || {
                pkg_name
                    .split('-')
                    .map(|w| {
                        let mut c = w.chars();
                        match c.next() {
                            None => String::new(),
                            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            },
            ToOwned::to_owned,
        );
    let theme = package
        .get("metadata")
        .and_then(|m| m.get("egui-web"))
        .and_then(|w| w.get("theme_color"))
        .and_then(|c| c.as_str())
        .unwrap_or("#0a0a0a")
        .to_owned();
    let short_name = package
        .get("metadata")
        .and_then(|m| m.get("egui-web"))
        .and_then(|w| w.get("short_name"))
        .and_then(|n| n.as_str())
        .map_or_else(|| pkg_name.to_owned(), ToOwned::to_owned);

    let index_template =
        std::fs::read_to_string("../egui-shadcn/templates/web/index.html").unwrap();
    let manifest_template =
        std::fs::read_to_string("../egui-shadcn/templates/web/manifest.json").unwrap();

    let index = index_template
        .replace("{{ title }}", &title)
        .replace("{{ theme_color }}", &theme)
        .replace("{{ pkg_js }}", &lib_name)
        .replace("{{ vsn }}", vsn);
    let manifest_rendered = manifest_template
        .replace("{{ title }}", &title)
        .replace("{{ short_name }}", &short_name)
        .replace("{{ theme_color }}", &theme);

    std::fs::create_dir_all("assets").unwrap();
    std::fs::write("assets/index.html", index).unwrap();
    std::fs::write("assets/manifest.webmanifest", manifest_rendered).unwrap();
    let _ = std::fs::copy("../egui-shadcn/assets/web/egui.js", "assets/egui.js").unwrap();
}
