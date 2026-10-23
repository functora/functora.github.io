//! An attach claim must stay held for the whole file-picker operation:
//! while it is held, no other op can claim the progress slot, and the picker's
//! completion must release it for the next claim.

use cryptonote_egui::app::CryptonoteApp;

fn ctx() -> egui::Context {
    egui::Context::default()
}

fn finish_pick(app: &mut CryptonoteApp) {
    let (tx, rx) = std::sync::mpsc::channel();
    app.pick_rx = Some(rx);
    assert!(tx.send(Ok(Vec::new())).is_ok(), "receiver must be alive");
    app.poll_receivers(&ctx());
}

#[test]
fn attach_claim_stays_held_until_completion() {
    let mut app = CryptonoteApp::default();
    assert!(app.begin_attach_for_tests(), "first attach claim must succeed");
    assert!(
        app.temporary.progress.is_some(),
        "claim must stay held while the attach op runs"
    );
    assert!(app.progress_overlay_open, "attach must open the progress overlay");
    finish_pick(&mut app);
    assert!(app.temporary.progress.is_none(), "completion must release the claim");
    assert!(!app.progress_overlay_open, "completion must close the overlay");
    assert!(
        app.begin_attach_for_tests(),
        "claim must be reacquirable after completion"
    );
}

#[test]
fn attach_claim_rejects_second_op_while_running() {
    let mut app = CryptonoteApp::default();
    assert!(app.begin_attach_for_tests(), "first attach claim must succeed");
    assert!(
        !app.begin_attach_for_tests(),
        "second claim must be rejected while the first op runs"
    );
}
