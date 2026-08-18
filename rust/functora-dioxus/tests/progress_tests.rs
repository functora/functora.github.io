#![allow(clippy::unwrap_used, clippy::expect_used)]
use dioxus::core::ScopeId;
use dioxus::prelude::{Signal, VirtualDom, rsx};
use functora_dioxus::progress::{Job, Stage, claim_job};

fn with_runtime<R>(body: impl FnOnce() -> R) -> R {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    dom.rebuild_in_place();
    dom.in_runtime(body)
}

fn progress() -> Signal<Option<Job<Stage>>> {
    Signal::new_in_scope(None, ScopeId(0))
}

#[test]
fn claim_job_first_claim_holds_slot() {
    with_runtime(|| {
        let progress = progress();
        let _claim = claim_job(progress, Stage::Decrypt).unwrap();
        assert!(progress().is_some());
    });
}

#[test]
fn claim_job_rejects_second_claim_while_first_held() {
    with_runtime(|| {
        let progress = progress();
        let _claim = claim_job(progress, Stage::Decrypt).unwrap();
        assert!(claim_job(progress, Stage::Encrypt).is_none());
        assert!(progress().is_some());
    });
}

#[test]
fn claim_job_releases_slot_on_drop() {
    with_runtime(|| {
        let progress = progress();
        {
            let _claim = claim_job(progress, Stage::Decrypt).unwrap();
        }
        assert!(progress().is_none());
        let _claim = claim_job(progress, Stage::Zip).unwrap();
        assert!(progress().is_some());
    });
}
