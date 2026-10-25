#![cfg(any(feature = "build", feature = "desktop"))]
#![cfg(not(target_arch = "wasm32"))]
use functora_egui::desktop::config::{iso_date_from_days_since_epoch, release_date};
use functora_egui::desktop::load_desktop_config;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

fn write_manifest(body: &str) -> tempfile::NamedTempFile {
    let file = tempfile::NamedTempFile::new().unwrap_or_else(|err| panic!("temp manifest: {err}"));
    std::fs::write(file.path(), body).unwrap_or_else(|err| panic!("seed manifest: {err}"));
    file
}

#[test]
fn defaults_derive_from_package() {
    let file = write_manifest("[package]\nname = \"my-app\"\nversion = \"1.2.3\"\n");
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.app_id, "io.functora.my_app");
    assert_eq!(cfg.title, "My App");
    assert_eq!(cfg.comment, "My App");
    assert_eq!(cfg.categories, "Utility;");
    assert!(cfg.mime_types.is_empty());
    assert!(cfg.schemes.is_empty());
    assert_eq!(cfg.version, "1.2.3");
    assert_eq!(cfg.exec_name, "my-app");
    assert_eq!(cfg.icon_name, cfg.app_id);
    assert_eq!(cfg.homepage, "https://functora.github.io/");
    assert!(cfg.developer.is_empty());
    assert_eq!(cfg.description, "My App");
    assert_eq!(cfg.developer_id, "io.functora.my_app");
}

#[test]
fn web_title_is_fallback_before_pkg_name() {
    let file = write_manifest(
        "[package]\nname = \"my-app\"\nversion = \"0.1.0\"\n[package.metadata.functora-egui-web]\ntitle = \"Web Title\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.title, "Web Title");
}

#[test]
fn explicit_metadata_overrides_everything() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"2.0.0\"\ndescription = \"pkg desc\"\n[package.metadata.functora-egui-desktop]\napp_id = \"io.example.Notes\"\ntitle = \"Notes\"\ncomment = \"Take notes\"\ncategories = \"Office;\"\nmime_types = \"application/x-notes;\"\nschemes = \"notes;\"\nhomepage = \"https://example.com/notes\"\ndeveloper = \"Example Org\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.app_id, "io.example.Notes");
    assert_eq!(cfg.title, "Notes");
    assert_eq!(cfg.comment, "Take notes");
    assert_eq!(cfg.categories, "Office;");
    assert_eq!(cfg.mime_types, "application/x-notes;");
    assert_eq!(cfg.schemes, "notes;");
    assert_eq!(cfg.version, "2.0.0");
    assert_eq!(cfg.exec_name, "pkg");
    assert_eq!(cfg.icon_name, "io.example.Notes");
    assert_eq!(cfg.homepage, "https://example.com/notes");
    assert_eq!(cfg.developer, "Example Org");
}

#[test]
fn comment_falls_back_to_description() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\ndescription = \"Pkg description\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.comment, "Pkg description");
}

#[test]
fn description_falls_back_to_comment() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n[package.metadata.functora-egui-desktop]\ncomment = \"Short pitch\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.description, "Short pitch");
}

#[test]
fn description_and_developer_id_can_be_overridden() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n[package.metadata.functora-egui-desktop]\ndescription = \"Long pitch\"\ndeveloper_id = \"example.org\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.description, "Long pitch");
    assert_eq!(cfg.developer_id, "example.org");
}

#[test]
fn homepage_prefers_homepage_over_repository() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\nhomepage = \"https://example.com/home\"\nrepository = \"https://example.com/repo\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.homepage, "https://example.com/home");
}

#[test]
fn homepage_falls_back_to_repository() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\nrepository = \"https://example.com/repo\"\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.homepage, "https://example.com/repo");
}

#[test]
fn developer_strips_email_from_first_author() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\nauthors = [\"Example Org <team@example.com>\"]\n",
    );
    let path = file
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("manifest path"));
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.developer, "Example Org");
}

#[test]
fn iso_date_conversion_handles_epoch_leap_day_and_year_boundary() {
    assert_eq!(iso_date_from_days_since_epoch(0), "1970-01-01");
    assert_eq!(iso_date_from_days_since_epoch(10956), "1999-12-31");
    assert_eq!(iso_date_from_days_since_epoch(10957), "2000-01-01");
    assert_eq!(iso_date_from_days_since_epoch(19782), "2024-02-29");
    assert_eq!(iso_date_from_days_since_epoch(20734), "2026-10-08");
}

#[test]
fn release_date_is_iso_formatted() {
    let date = release_date();
    assert_eq!(date.len(), 10);
    assert_eq!(date.as_bytes()[4], b'-');
    assert_eq!(date.as_bytes()[7], b'-');
    assert!(
        date.bytes()
            .enumerate()
            .filter(|(i, _)| *i != 4 && *i != 7)
            .all(|(_, b)| b.is_ascii_digit()),
        "unexpected date: {date}"
    );
}

fn is_leap(year: u32) -> bool {
    (year.is_multiple_of(4) && !year.is_multiple_of(100)) || year.is_multiple_of(400)
}

fn days_in_month(year: u32, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap(year) => 29,
        2 => 28,
        _ => 0,
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn iso_date_is_always_a_valid_calendar_date(days in 0..3_000_000_u64) {
        let rendered = iso_date_from_days_since_epoch(days);
        let mut parts = rendered.split('-');
        let year: u32 = parts.next().unwrap_or_else(|| panic!("year field")).parse().unwrap_or_else(|err| panic!("year digits: {err}"));
        let month: u32 = parts.next().unwrap_or_else(|| panic!("month field")).parse().unwrap_or_else(|err| panic!("month digits: {err}"));
        let day: u32 = parts.next().unwrap_or_else(|| panic!("day field")).parse().unwrap_or_else(|err| panic!("day digits: {err}"));
        prop_assert!(parts.next().is_none(), "date must have exactly 3 fields");
        prop_assert!((1..=12).contains(&month), "month {month} out of range");
        prop_assert!(
            (1..=days_in_month(year, month)).contains(&day),
            "day {day} out of range for {year}-{month:02}"
        );
    }
}
