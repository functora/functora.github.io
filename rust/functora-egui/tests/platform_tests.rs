#![allow(clippy::unwrap_used, clippy::expect_used)]
use functora_egui::{deep_link, files, nav::NavHistory, progress, storage, theme_extra::Theme};
use std::sync::{Mutex, OnceLock, PoisonError};

fn deep_link_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

#[test]
fn storage_roundtrip() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("storage.json");
    let key = "unit_test_key";
    let value = serde_json::json!({"a": 1});
    assert!(storage::update_key(&path, key, &value).is_ok());
    let loaded: Option<serde_json::Value> = (|| {
        let json = storage::read_json_object(&path).ok()?;
        let v = json.get(key)?;
        serde_json::from_value(v.clone()).ok()
    })();
    assert_eq!(loaded, Some(value));
}

#[test]
fn storage_persistent_wrapper() {
    let _guard = deep_link_lock()
        .lock()
        .unwrap_or_else(PoisonError::into_inner);
    let mut p: storage::Persistent<u32> =
        storage::Persistent::new("test_persistent_u32_clippy", 42);
    assert_eq!(*p.get(), 42);
    p.set(99);
    assert_eq!(*p.get(), 99);
    let q: storage::Persistent<u32> = storage::Persistent::new("test_persistent_u32_clippy", 0);
    assert_eq!(*q.get(), 99);
    let mut r = q;
    r.set(42);
    assert_eq!(*r.get(), 42);
}

#[test]
fn deep_link_store_take() {
    let _guard = deep_link_lock()
        .lock()
        .unwrap_or_else(PoisonError::into_inner);
    deep_link::store_url("https://example.com/?foo=bar".into());
    let taken = deep_link::take_url();
    assert_eq!(taken, Some("https://example.com/?foo=bar".to_string()));
    assert_eq!(deep_link::take_url(), None);
}

#[test]
fn deep_link_url_to_route() {
    assert_eq!(
        deep_link::url_to_route("https://example.com/?route=/home"),
        Some("/?route=/home".to_string())
    );
    assert_eq!(deep_link::url_to_route("https://example.com/"), None);
}

#[test]
fn deep_link_poll() {
    let _guard = deep_link_lock()
        .lock()
        .unwrap_or_else(PoisonError::into_inner);
    deep_link::store_url("https://example.com/?x=1".into());
    let polled = deep_link::poll_deep_link();
    assert!(polled.is_some());
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum Route {
    #[default]
    Home,
    About,
    Settings,
}

impl std::fmt::Display for Route {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Home => write!(f, "home"),
            Self::About => write!(f, "about"),
            Self::Settings => write!(f, "settings"),
        }
    }
}

impl std::str::FromStr for Route {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "/about" | "about" => Ok(Route::About),
            "/settings" | "settings" => Ok(Route::Settings),
            "/" | "home" => Ok(Route::Home),
            _ => Err("unknown".into()),
        }
    }
}

#[test]
fn nav_stack_push_and_back() {
    let mut nav: NavHistory<Route> = NavHistory::new(Route::Home);
    assert_eq!(nav.current(), &Route::Home);
    assert!(!nav.can_go_back());
    nav.push(Route::About);
    assert_eq!(nav.current(), &Route::About);
    assert!(nav.can_go_back());
    _ = nav.go_back();
    assert_eq!(nav.current(), &Route::Home);
    _ = nav.go_back();
}

#[test]
fn nav_stack_push_route() {
    let mut nav: NavHistory<Route> = NavHistory::new(Route::Home);
    nav.push(Route::About);
    assert_eq!(nav.current(), &Route::About);
    nav.push(Route::Settings);
    assert_eq!(nav.current(), &Route::Settings);
}

#[test]
fn nav_stack_reset() {
    let mut nav: NavHistory<Route> = NavHistory::new(Route::Home);
    nav.push(Route::About);
    nav.push(Route::Settings);
    nav.truncate_forward();
    assert_eq!(nav.current(), &Route::Settings);
    _ = nav.go_back();
    _ = nav.go_back();
    assert_eq!(nav.current(), &Route::Home);
}

#[test]
fn progress_report_and_claim() {
    let mut slot: Option<progress::Job<progress::Stage>> = None;
    progress::report(&mut slot, progress::Stage::Attach, 5, 10);
    assert_eq!(slot.as_ref().unwrap().done, 5);
    progress::clear_progress(&mut slot);
    assert!(slot.is_none());
    let guard = progress::claim_job(&mut slot, progress::Stage::Zip);
    assert!(guard.is_some());
    drop(guard);
    assert!(slot.is_none());
    let guard2 = progress::claim_job(&mut slot, progress::Stage::Zip);
    assert!(guard2.is_some());
}

#[test]
fn progress_job_percent() {
    let job = progress::Job {
        stage: progress::Stage::Download,
        done: 50,
        total: 100,
        name: None,
    };
    assert_eq!(job.percent(), 50);
    let job2 = progress::Job {
        stage: progress::Stage::Download,
        done: 10,
        total: 0,
        name: Some("file".into()),
    };
    assert_eq!(job2.percent(), 100);
}

#[test]
fn files_mime_and_preview() {
    assert_eq!(files::mime_for_name("photo.jpg"), Some("image/jpeg"));
    assert_eq!(files::mime_for_name("video.mp4"), Some("video/mp4"));
    assert_eq!(files::mime_for_name("unknown.xyz"), None);
    assert_eq!(files::format_size(512), "512 B");
    assert_eq!(files::format_size(2048), "2.0 KB");
    let preview = files::preview("hello.txt", b"hello world");
    assert!(matches!(preview, files::Preview::Text(t) if t == "hello world"));
    let preview2 = files::preview("image.png", b"fake");
    assert!(matches!(preview2, files::Preview::Image(_)));
}

#[test]
fn files_preview_blob_and_revoke() {
    let preview = files::preview_blob("photo.jpg", b"fake image data");
    assert!(matches!(preview, files::Preview::Image(_)));
    if let files::Preview::Image(url) = preview {
        assert!(files::revoke_blob_url(&url).is_ok());
    }
    let preview2 = files::preview_blob("doc.txt", b"text");
    assert!(matches!(preview2, files::Preview::Text(_)));
}

#[test]
fn files_data_url_mime() {
    assert_eq!(
        files::data_url_mime("data:image/png;base64"),
        Some("image/png")
    );
    assert_eq!(files::data_url_mime("data:;base64"), None);
    assert_eq!(files::data_url_mime("not_data"), None);
}

#[test]
fn theme_next_and_set() {
    assert_eq!(Theme::Light.next(), Theme::Dark);
    assert_eq!(Theme::Dark.next(), Theme::Light);
    assert_eq!(Theme::Light.as_str(), "light");
    assert_eq!(Theme::Dark.to_string(), "Dark");
    let ctx = egui::Context::default();
    functora_egui::theme_extra::set_theme(&ctx, Theme::Dark);
    assert_eq!(functora_egui::theme_extra::current_theme(&ctx), Theme::Dark);
    functora_egui::theme_extra::set_theme(&ctx, Theme::Light);
    assert_eq!(
        functora_egui::theme_extra::current_theme(&ctx),
        Theme::Light
    );
}

#[test]
fn pwa_js_generation() {
    let js = functora_egui::pwa::pwa_init_js("/sw.js", "app-v1");
    assert!(js.contains("/sw.js"));
    assert!(js.contains("app-v1"));
    let sw = functora_egui::pwa::pwa_sw_js("app-v1", &["/index.html"]);
    assert!(sw.contains("app-v1"));
    assert!(sw.contains("/index.html"));
}

#[test]
fn in_flight_guard() {
    let flight = functora_egui::in_flight::InFlight::new();
    assert!(!flight.is_in_flight());
    let guard = flight.claim();
    assert!(guard.is_some());
    assert!(flight.is_in_flight());
    assert!(flight.claim().is_none());
    drop(guard);
    assert!(!flight.is_in_flight());
    assert!(flight.claim().is_some());
}

#[test]
fn config_derive() {
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let path = tmp.path().to_str().unwrap();
    std::fs::write(
        path,
        r##"
[package]
name = "my-app"
version = "1.2.3"
[lib]
name = "my_app"
[package.metadata.functora-egui-web]
title = "Custom Title"
theme_color = "#ff0000"
"##,
    )
    .unwrap();
    assert_eq!(
        functora_egui::config::shared::derive_pkg_js(path),
        "my_app.js"
    );
    assert_eq!(functora_egui::web::derive_title(path), "Custom Title");
    assert_eq!(functora_egui::web::derive_theme_color(path), "#ff0000");
    let cfg = functora_egui::web::load_config(path);
    assert_eq!(cfg.title, "Custom Title");
    assert_eq!(cfg.vsn, "1.2.3");
    let android_cfg = functora_egui::android::load_android_config(path);
    assert_eq!(android_cfg.version_name, "1.2.3");
    assert_eq!(android_cfg.version_code, 10203);
}

#[test]
fn encoding_helpers() {
    #[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
    struct Payload {
        x: u32,
    }
    let v = Payload { x: 42 };
    let enc = functora_egui::encoding::encode_payload(&v).unwrap();
    let dec: Payload = functora_egui::encoding::decode_payload(&enc).unwrap();
    assert_eq!(v, dec);
    let url = functora_egui::encoding::append_query_param("https://example.com", "k", "v");
    assert!(url.contains("k=v"));
    let extracted = functora_egui::encoding::extract_query_param(&url, "k");
    assert_eq!(extracted, Some("v".to_string()));
}

#[test]
fn state_default() {
    let s = functora_egui::state::PersistentState::default();
    assert_eq!(s.theme, Theme::Light);
}

#[test]
fn error_display() {
    let err = functora_egui::error::Error::JS("oops".into());
    assert!(err.to_string().contains("oops"));
}
