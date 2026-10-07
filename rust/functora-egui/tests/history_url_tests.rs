use functora_egui::route::resolve_history_url;

#[test]
fn history_preserves_versioned_path() {
    assert_eq!(
        resolve_history_url("/apps/functora-egui-demo/0.1.0/", "/?screen=button"),
        "/apps/functora-egui-demo/0.1.0/?screen=button"
    );
    assert_eq!(
        resolve_history_url("/apps/functora-egui-demo/0.1.0/", "/"),
        "/apps/functora-egui-demo/0.1.0/"
    );
    assert_eq!(
        resolve_history_url("/", "/?screen=button"),
        "/?screen=button"
    );
    assert_eq!(resolve_history_url("/", "/"), "/");
}

#[test]
fn history_passes_through_other_urls() {
    assert_eq!(
        resolve_history_url(
            "/apps/functora-egui-demo/0.1.0/",
            "https://example.com/?screen=button"
        ),
        "https://example.com/?screen=button"
    );
}
