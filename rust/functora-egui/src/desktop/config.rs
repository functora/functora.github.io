use crate::config::shared::{capitalize_words, metadata_str, parse_toml, pkg_name};

#[derive(Debug, Clone)]
pub struct DesktopConfig {
    pub app_id: String,
    pub title: String,
    pub comment: String,
    pub description: String,
    pub categories: String,
    pub mime_types: String,
    pub schemes: String,
    pub version: String,
    pub exec_name: String,
    pub icon_name: String,
    pub homepage: String,
    pub developer: String,
    pub developer_id: String,
}

fn derive_app_id(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "app_id").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(pkg_name)
            .map_or_else(
                || "io.functora.app".to_owned(),
                |name| format!("io.functora.{}", name.replace('-', "_")),
            )
    })
}

fn derive_title(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "title")
        .or_else(|| metadata_str(manifest_path, "functora-egui-web", "title"))
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
        })
}

fn derive_comment(manifest_path: &str, title: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "comment").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(|value| {
                value
                    .get("package")
                    .and_then(|pkg| pkg.get("description"))
                    .and_then(|d| d.as_str())
            })
            .map_or_else(|| title.to_owned(), ToOwned::to_owned)
    })
}

fn derive_description(manifest_path: &str, comment: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "description")
        .unwrap_or_else(|| comment.to_owned())
}

fn derive_categories(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "categories")
        .unwrap_or_else(|| "Utility;".to_owned())
}

fn derive_mime_types(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "mime_types").unwrap_or_default()
}

fn derive_schemes(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "schemes").unwrap_or_default()
}

fn derive_version(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("version"))
                .and_then(|v| v.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| "0.0.0".to_owned())
}

fn derive_exec_name(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(pkg_name)
        .unwrap_or_else(|| "app".to_owned())
}

fn derive_homepage(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "homepage").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(|value| value.get("package"))
            .and_then(|pkg| {
                pkg.get("homepage")
                    .or_else(|| pkg.get("repository"))
                    .and_then(|v| v.as_str())
            })
            .map_or_else(
                || "https://functora.github.io/".to_owned(),
                ToOwned::to_owned,
            )
    })
}

fn author_name(raw: &str) -> String {
    raw.split('<')
        .next()
        .map_or_else(|| raw.to_owned(), |head| head.trim().to_owned())
}

fn derive_developer(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "developer").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(|value| value.get("package"))
            .and_then(|pkg| pkg.get("authors"))
            .and_then(|authors| authors.as_array())
            .and_then(|list| list.first())
            .and_then(|first| first.as_str())
            .map_or_else(String::new, author_name)
    })
}

#[must_use]
pub fn iso_date_from_days_since_epoch(days: u64) -> String {
    let base = days + 719_468;
    let era = base / 146_097;
    let day_of_era = base - era * 146_097;
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let year = year_of_era + era * 400;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_marker = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_marker + 2) / 5 + 1;
    let month = if month_marker < 10 {
        month_marker + 3
    } else {
        month_marker - 9
    };
    let full_year = if month <= 2 { year + 1 } else { year };
    format!("{full_year:04}-{month:02}-{day:02}")
}

#[must_use]
pub fn release_date() -> String {
    std::env::var("SOURCE_DATE_EPOCH")
        .ok()
        .and_then(|raw| raw.parse::<u64>().ok())
        .map(|epoch| epoch / 86_400)
        .or_else(|| {
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .ok()
                .map(|elapsed| elapsed.as_secs() / 86_400)
        })
        .map_or_else(|| "1970-01-01".to_owned(), iso_date_from_days_since_epoch)
}

#[must_use]
pub fn load_desktop_config(manifest_path: &str) -> DesktopConfig {
    let app_id = derive_app_id(manifest_path);
    let title = derive_title(manifest_path);
    let comment = derive_comment(manifest_path, &title);
    let description = derive_description(manifest_path, &comment);
    let developer_id = metadata_str(manifest_path, "functora-egui-desktop", "developer_id")
        .unwrap_or_else(|| app_id.clone());
    DesktopConfig {
        icon_name: app_id.clone(),
        app_id,
        exec_name: derive_exec_name(manifest_path),
        version: derive_version(manifest_path),
        categories: derive_categories(manifest_path),
        mime_types: derive_mime_types(manifest_path),
        schemes: derive_schemes(manifest_path),
        homepage: derive_homepage(manifest_path),
        developer: derive_developer(manifest_path),
        developer_id,
        description,
        comment,
        title,
    }
}
