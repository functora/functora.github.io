//! The `InFlight` demo must actually hold its claim for the whole simulated
//! operation instead of dropping the guard at the end of the click handler.

use functora_egui_demo::ShowcaseApp;

fn ctx() -> egui::Context {
    egui::Context::default()
}

#[test]
fn claim_stays_held_after_demo_click() {
    let mut state = ShowcaseApp::default();
    assert!(!state.platform.in_flight.is_in_flight());
    assert!(state.try_claim_in_flight(&ctx()));
    assert!(
        state.platform.in_flight.is_in_flight(),
        "guard must be held by the running task, not dropped with the click handler"
    );
    assert!(
        !state.try_claim_in_flight(&ctx()),
        "a second claim during the hold must be rejected"
    );
}
