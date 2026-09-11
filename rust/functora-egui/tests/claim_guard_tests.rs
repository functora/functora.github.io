//! Claim-guard lifecycles (plan step 5b).
//!
//! `InFlight` (shared atomic flag) and `progress::claim_job` (exclusive slot)
//! intentionally stay separate types: one is `Clone + Send` shared across
//! threads, the other is an exclusive `&mut` borrow. These tests lock the
//! lifecycle both must honor: second claim fails while held, succeeds after
//! the guard drops.

#![allow(clippy::unwrap_used, clippy::expect_used)]

fn assert_send_sync<T>()
where
    T: Send + Sync,
{
}

#[test]
fn in_flight_claim_holds_until_guard_drops() {
    let flight = functora_egui::in_flight::InFlight::new();
    assert_send_sync::<functora_egui::in_flight::InFlight>();
    assert!(!flight.is_in_flight());
    let guard = flight.claim().expect("first claim must succeed");
    assert!(flight.is_in_flight());
    assert!(flight.claim().is_none(), "second claim must fail");
    drop(guard);
    assert!(!flight.is_in_flight());
    assert!(flight.claim().is_some(), "claim must succeed after drop");
}

#[test]
fn in_flight_clone_shares_one_flag() {
    let flight = functora_egui::in_flight::InFlight::new();
    let shared = flight.clone();
    let _guard = flight.claim().expect("first claim must succeed");
    assert!(shared.claim().is_none(), "clone must see the claim");
    assert!(shared.is_in_flight());
}

#[test]
fn claim_job_holds_slot_until_guard_drops() {
    use functora_egui::progress::{Job, Stage, claim_job};
    let mut slot: Option<Job<Stage>> = None;
    // While the guard lives, `slot` is exclusively borrowed (a second
    // `claim_job(&mut slot, …)` does not even compile) — exclusivity is
    // enforced by the borrow checker itself.
    {
        let _guard = claim_job(&mut slot, Stage::Zip).expect("first claim must succeed");
    }
    assert!(slot.is_none(), "drop must clear the slot");
    {
        let _guard = claim_job(&mut slot, Stage::Unzip).expect("reclaim must succeed");
        // Still held here; releasing at scope end.
    }
    assert!(slot.is_none(), "drop must clear the slot again");
}
