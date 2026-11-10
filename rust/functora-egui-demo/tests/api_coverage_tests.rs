//! Regression tests for the public API coverage of the showcase app.
//!
//! The demo must teach the whole public surface of `functora-egui` and
//! `functora-core`. These tests lock in the parts that break silently:
//! the Utilities screen registration and rendering, the manifest helpers that
//! must not re-read the manifest on every frame, and the snippet literals
//! that must contain real line breaks instead of a visible `\n`.

use std::path::{Path, PathBuf};

use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

const APP_SRC: &str = include_str!("../src/app.rs");
const STATE_SRC: &str = include_str!("../src/state.rs");
const UTILITIES_SRC: &str = include_str!("../src/screens/platform/utilities.rs");
const DISPLAY_IMAGE_SRC: &str = include_str!("../src/screens/display/image.rs");
const PLATFORM_DEEP_LINK_SRC: &str = include_str!("../src/screens/platform/deep_link.rs");
const PLATFORM_PROGRESS_WORKER_SRC: &str =
    include_str!("../src/screens/platform/progress_worker.rs");
const PLATFORM_THUMBNAIL_SRC: &str = include_str!("../src/screens/platform/thumbnail.rs");
const PLATFORM_ZIP_SRC: &str = include_str!("../src/screens/platform/zip.rs");

/// Files whose snippets deliberately show Rust escape sequences as part of
/// the demonstrated code (`println!("...")`, markdown headers, ...).
const ESCAPE_SHOWCASE_FILES: &[&str] = &[
    "src/screens/display/code_snippet.rs",
    "src/screens/platform/markdown.rs",
];

/// The utilities screen is taller than a laptop viewport and egui only paints
/// labels that intersect the clip rect, so the test renders into a viewport
/// that fits the whole screen.
const TEST_VIEWPORT: egui::Vec2 = egui::vec2(1280.0, 8000.0);

fn rendered_app_with_state<F>(mut body: F) -> (String, ShowcaseApp)
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let ctx = egui::Context::default();
    let mut app = ShowcaseApp::default();
    let mut out = ctx.run_ui(
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(egui::Pos2::ZERO, TEST_VIEWPORT)),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(&mut app, inner));
        },
    );
    out.textures_delta.clear();
    let text = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n");
    (text, app)
}

fn rendered_app<F>(body: F) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    rendered_app_with_state(body).0
}

fn position(haystack: &str, needle: &str) -> usize {
    haystack
        .find(needle)
        .unwrap_or_else(|| panic!("{needle} must appear in the utilities screen"))
}

fn relative(path: &Path, root: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn rust_sources(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let entries = std::fs::read_dir(dir)?
        .map(|entry| entry.map(|dir_entry| dir_entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?;
    let (dirs, files): (Vec<PathBuf>, Vec<PathBuf>) =
        entries.into_iter().partition(|path| path.is_dir());
    Ok(files
        .into_iter()
        .filter(|path| path.extension().is_some_and(|ext| ext == "rs"))
        .chain(
            dirs.into_iter()
                .map(|path| rust_sources(&path))
                .collect::<std::io::Result<Vec<_>>>()?
                .into_iter()
                .flatten(),
        )
        .collect())
}

#[test]
fn utilities_demo_is_registered_catalog_entry() {
    let id = ComponentId::Utilities;
    assert!(
        ComponentId::ALL.contains(&id),
        "{id:?} must be in ComponentId::ALL"
    );
    assert_eq!(ComponentId::from_slug(&id.slug()), Some(id));
    let matches = CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .filter(|def| def.name == id.name())
        .count();
    assert_eq!(matches, 1, "{id:?} must have exactly one catalog entry");
}

#[test]
fn utilities_demo_is_dispatched_from_the_app() {
    assert!(
        APP_SRC.contains("Some(ComponentId::Utilities) => self.demo_utilities(ui)"),
        "app.rs must dispatch the Utilities catalog entry"
    );
}

#[test]
fn utilities_demo_renders_every_helper_section() {
    let text = rendered_app(ShowcaseApp::demo_utilities);
    for section in [
        "cast: saturating numeric conversions",
        "frame: camera buffers and preview sizing",
        "text: measurement and wrapping",
        "paint: shared painting helpers",
        "tokens: design tokens",
        "style resolution: the exact colors a widget paints",
        "interaction: pointer and enablement state",
        "theme: light/dark detection",
        "utils: compatibility facade",
        "config / icon_export / build",
        "packaging: per-platform build configuration",
        "packaging templates",
        "packaging icons",
        "platform runtime: back handling and startup",
    ] {
        assert!(
            text.contains(section),
            "utilities demo must render the {section} section"
        );
    }
}

#[test]
fn utilities_demo_shows_the_public_helper_results() {
    let text = rendered_app(ShowcaseApp::demo_utilities);
    for result in [
        "f32_to_u8_clamped(300.0) = 255",
        "cap_to_available(300.0, 200.0) = 200",
        "ResolvedButtonStyle: bg",
        "Interaction::IDLE: hovered",
        "default_theme(ctx)",
        "WebConfig: title",
        "AndroidConfig: namespace",
        "DesktopConfig: app_id",
        "IndexHtml: title",
        "Metainfo: app_id",
    ] {
        assert!(
            text.contains(result),
            "utilities demo must show {result}: {text}"
        );
    }
}

#[test]
fn utilities_demo_does_not_reparse_the_manifest_per_frame() {
    for helper in [
        "functora_egui::config::shared::parse_toml(",
        "functora_egui::web::config::load_config(",
        "functora_egui::android::config::load_android_config(",
        "functora_egui::desktop::config::load_desktop_config(",
        "functora_egui::web::config::derive_title(",
        "functora_egui::web::config::derive_theme_color(",
        "functora_egui::desktop::config::release_date()",
    ] {
        assert!(
            !UTILITIES_SRC.contains(helper),
            "{helper} must not run on every rendered frame"
        );
    }
}

#[test]
fn showcase_app_default_does_not_read_the_manifest_or_the_clock() {
    let app = ShowcaseApp::default();
    assert!(
        !app.platform.packaging.is_loaded(),
        "starting the app must not parse the manifest or read the clock"
    );
}

#[test]
fn utilities_demo_loads_the_packaging_metadata_on_demand() {
    let (text, app) = rendered_app_with_state(ShowcaseApp::demo_utilities);
    assert!(
        app.platform.packaging.is_loaded(),
        "the utilities screen must load the packaging metadata it shows"
    );
    assert!(
        text.contains("manifest parsed on demand"),
        "the utilities screen must show which manifest it parsed: {text}"
    );
}

#[test]
fn packaging_metadata_is_cached_after_the_first_load() {
    let mut app = ShowcaseApp::default();
    let release_date = app.platform.packaging.metadata().release_date.clone();
    assert!(app.platform.packaging.is_loaded());
    assert_eq!(
        app.platform.packaging.metadata().release_date,
        release_date,
        "the packaging metadata must be cached, not parsed again on every frame"
    );
    assert!(
        STATE_SRC.contains("get_or_insert_with"),
        "the packaging state must cache the parsed metadata"
    );
}

#[test]
fn utilities_demo_only_writes_storage_on_demand() {
    let trigger = position(UTILITIES_SRC, "Write storage value");
    let write = position(UTILITIES_SRC, "storage_set(");
    assert!(
        trigger < write,
        "storage_set must run only after the user asks for it"
    );
}

#[test]
fn utilities_demo_only_wakes_the_event_loop_on_demand() {
    let trigger = position(UTILITIES_SRC, "Store context and wake");
    let wake = position(UTILITIES_SRC, "wake_via_repaint()");
    assert!(
        trigger < wake,
        "wake_via_repaint must run only after the user asks for it, otherwise the demo repaints forever"
    );
}

#[test]
fn snippet_sources_never_render_visible_escape_sequences() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let sources = rust_sources(&root.join("src"))
        .unwrap_or_else(|err| panic!("cannot walk the demo sources: {err}"));
    let offenders = sources
        .iter()
        .filter(|path| !ESCAPE_SHOWCASE_FILES.contains(&relative(path, root).as_str()))
        .filter_map(|path| {
            let source = std::fs::read_to_string(path)
                .unwrap_or_else(|err| panic!("cannot read {}: {err}", path.display()));
            let count = source.matches("\\\\n").count();
            (count > 0).then(|| (relative(path, root), count))
        })
        .collect::<Vec<_>>();
    assert!(
        offenders.is_empty(),
        "snippet literals must use real line breaks, not a visible backslash-n: {offenders:?}"
    );
}

#[test]
fn image_snippet_keeps_real_line_breaks() {
    assert!(
        DISPLAY_IMAGE_SRC.contains(
            "expect(\\\"fixtures must encode\\\");\\n\\n// Every fixture knows its format"
        ),
        "the image snippet must separate the sample from the ImageType demo with real line breaks"
    );
}

#[test]
fn progress_worker_snippet_keeps_real_line_breaks() {
    assert!(
        PLATFORM_PROGRESS_WORKER_SRC.contains("lives\\n\\n// Render progress bar"),
        "the progress worker snippet must separate the InFlight demo with real line breaks"
    );
}

#[test]
fn zip_snippet_teaches_the_report_api() {
    assert!(
        PLATFORM_ZIP_SRC.contains("create_zip_report")
            && PLATFORM_ZIP_SRC.contains("unzip_report")
            && PLATFORM_ZIP_SRC.contains("ATTACHMENTS_PREFIX"),
        "the zip snippet must teach the report level API"
    );
}

#[test]
fn deep_link_snippet_teaches_the_web_poll() {
    assert!(
        PLATFORM_DEEP_LINK_SRC.contains("poll_deep_link_web"),
        "the deep link snippet must teach the web only poll"
    );
}

#[test]
fn thumbnail_snippet_teaches_the_core_helpers() {
    assert!(
        PLATFORM_THUMBNAIL_SRC.contains("cached_thumbnail")
            && PLATFORM_THUMBNAIL_SRC.contains("jpeg_data_url"),
        "the thumbnail snippet must teach the memoized core helpers"
    );
}
