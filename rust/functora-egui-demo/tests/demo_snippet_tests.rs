//! Regression tests for demo snippet accuracy (plan step 2).
//!
//! Snippet strings duplicate live code and drift. The command palette snippet
//! must use the same `Command::with_items` + `CommandItem` API as the live
//! overlay in `app.rs`, not the stale tuple `Command::new` form.

const OVERLAY_SRC: &str = include_str!("../src/sections/overlay.rs");
const APP_SRC: &str = include_str!("../src/app.rs");

#[test]
fn live_overlay_uses_with_items_api() {
    assert!(
        APP_SRC.contains("Command::with_items"),
        "live app.rs must build the palette with Command::with_items"
    );
}

#[test]
fn command_snippet_matches_live_with_items_api() {
    assert!(
        OVERLAY_SRC.contains("Command::with_items"),
        "demo_command snippet must use Command::with_items like live app.rs"
    );
    assert!(
        OVERLAY_SRC.contains("CommandItem"),
        "demo_command snippet must mention CommandItem like live app.rs"
    );
}
