use functora_egui::pwa::{pwa_init_js, pwa_sw_js};

#[test]
fn pwa_init_js_escapes_special_chars() {
    let js = pwa_init_js("sw'te\\st.js\nline", "cach'e\nname\\");
    assert!(js.contains("sw\\'te\\\\st.js\\nline"));
    assert!(js.contains("cach\\'e\\nname\\\\"));
    assert!(!js.contains("sw'te"));
    assert!(!js.contains('\n'));
}

#[test]
fn pwa_init_js_plain_values_unchanged() {
    let js = pwa_init_js("sw.js", "v1");
    assert!(js.contains("register('sw.js')"));
    assert!(js.contains("__functoraCacheName='v1'"));
}

#[test]
fn pwa_sw_js_escapes_cache_name_and_assets() {
    let js = pwa_sw_js("cac'he\n", &["a's\\b.js", "plain.js"]);
    assert!(js.contains("cac\\'he\\n"));
    assert!(js.contains("'a\\'s\\\\b.js'"));
    assert!(js.contains("'plain.js'"));
    assert!(!js.contains('\n'));
}
