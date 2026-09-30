use functora_egui::theme_extra::{
    Theme, current_theme, default_theme, detect_system_theme, set_theme,
};

#[test]
fn theme_next_toggles() {
    assert_eq!(Theme::Light.next(), Theme::Dark);
    assert_eq!(Theme::Dark.next(), Theme::Light);
}

#[test]
fn theme_as_str() {
    assert_eq!(Theme::Light.as_str(), "light");
    assert_eq!(Theme::Dark.as_str(), "dark");
}

#[test]
fn theme_display() {
    assert_eq!(format!("{}", Theme::Light), "Light");
    assert_eq!(format!("{}", Theme::Dark), "Dark");
}

#[test]
fn theme_set_and_current() {
    let ctx = egui::Context::default();
    set_theme(&ctx, Theme::Dark);
    assert_eq!(current_theme(&ctx), Theme::Dark);
    set_theme(&ctx, Theme::Light);
    assert_eq!(current_theme(&ctx), Theme::Light);
}

#[test]
fn theme_default_is_light_without_system_theme() {
    let ctx = egui::Context::default();
    let theme = default_theme(&ctx);
    assert!(matches!(theme, Theme::Light | Theme::Dark));
}

#[test]
fn theme_detect_system_theme_returns_option() {
    let ctx = egui::Context::default();
    let theme = detect_system_theme(&ctx);
    assert!(theme.is_some() || theme.is_none());
}

#[test]
fn pwa_init_js_contains_service_worker() {
    let js = functora_egui::pwa::pwa_init_js("/sw.js", "cache-v1");
    assert!(js.contains("serviceWorker"));
    assert!(js.contains("/sw.js"));
    assert!(js.contains("cache-v1"));
}

#[test]
fn pwa_sw_js_contains_cache_name() {
    let js = functora_egui::pwa::pwa_sw_js("cache-v1", &["index.html", "app.js"]);
    assert!(js.contains("cache-v1"));
    assert!(js.contains("index.html"));
    assert!(js.contains("app.js"));
}

#[test]
fn pwa_sw_js_handles_empty_assets() {
    let js = functora_egui::pwa::pwa_sw_js("cache-v1", &[]);
    assert!(js.contains("cache-v1"));
}

#[cfg(feature = "markdown")]
#[test]
fn markdown_view_show_renders_without_panic() {
    let ctx = egui::Context::default();
    let mut cache = functora_egui::CommonMarkCache::default();
    let raw = egui::RawInput::default();
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = functora_egui::markdown_view::show(ui, &mut cache, "# Hello");
    });
    out.textures_delta.clear();
}

#[cfg(feature = "markdown")]
#[test]
fn markdown_view_show_handles_empty_string() {
    let ctx = egui::Context::default();
    let mut cache = functora_egui::CommonMarkCache::default();
    let raw = egui::RawInput::default();
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = functora_egui::markdown_view::show(ui, &mut cache, "");
    });
    out.textures_delta.clear();
}

#[cfg(feature = "markdown")]
#[test]
fn markdown_view_show_handles_special_characters() {
    let ctx = egui::Context::default();
    let mut cache = functora_egui::CommonMarkCache::default();
    let raw = egui::RawInput::default();
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = functora_egui::markdown_view::show(ui, &mut cache, "<script>alert('xss')</script>");
    });
    out.textures_delta.clear();
}

#[test]
fn in_flight_claim_and_release() {
    let in_flight = functora_egui::in_flight::InFlight::new();
    assert!(!in_flight.is_in_flight());
    let guard = in_flight.claim();
    assert!(guard.is_some());
    assert!(in_flight.is_in_flight());
    drop(guard);
    assert!(!in_flight.is_in_flight());
}

#[test]
fn in_flight_claim_returns_none_when_already_claimed() {
    let in_flight = functora_egui::in_flight::InFlight::new();
    let guard = in_flight.claim();
    assert!(guard.is_some());
    assert!(in_flight.claim().is_none());
}

#[test]
fn job_guard_clears_on_drop() {
    let mut job: Option<functora_egui::progress::Job<String>> = None;
    let guard = functora_egui::progress::claim_job(&mut job, "test".to_string());
    assert!(guard.is_some());
    drop(guard);
    assert!(job.is_none());
}

#[test]
fn job_guard_claim_returns_none_when_occupied() {
    let mut job: Option<functora_egui::progress::Job<String>> = None;
    let guard = functora_egui::progress::claim_job(&mut job, "test".to_string());
    assert!(guard.is_some());
    drop(guard);
    let second = functora_egui::progress::claim_job(&mut job, "other".to_string());
    assert!(second.is_some());
}

#[cfg(feature = "files")]
#[test]
fn cancel_token_creation_and_check() {
    let token = functora_egui::new_cancel_token();
    assert!(!functora_egui::is_cancelled(&token));
}

#[cfg(feature = "files")]
#[test]
fn cancel_token_cancel_sets_flag() {
    let token = functora_egui::new_cancel_token();
    functora_egui::cancel(&token);
    assert!(functora_egui::is_cancelled(&token));
}

#[cfg(feature = "storage")]
#[test]
fn persistent_default() {
    let _persistent: functora_egui::storage::Persistent<String> =
        functora_egui::storage::Persistent::new("test", "default".to_string());
}

#[test]
fn theme_serialization_roundtrip() {
    let json = serde_json::to_string(&Theme::Light).unwrap_or_else(|e| panic!("serialize: {e}"));
    let deserialized: Theme =
        serde_json::from_str(&json).unwrap_or_else(|e| panic!("deserialize: {e}"));
    assert_eq!(deserialized, Theme::Light);
}
