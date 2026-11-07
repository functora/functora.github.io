use functora_core::progress::{Job, Stage};

fn job(done: u64, total: u64) -> Job<Stage> {
    Job {
        stage: Stage::Zip,
        done,
        total,
        name: None,
    }
}

#[test]
fn percent_is_never_truncated_or_panic_on_extreme_counts() {
    assert_eq!(job(u64::MAX, u64::MAX).percent(), 100);
    assert_eq!(job(u64::MAX, 1).percent(), 100);
    assert_eq!(job(0, u64::MAX).percent(), 0);
}

#[test]
fn percent_clamps_zero_total_to_full_progress() {
    assert_eq!(job(1, 0).percent(), 100);
    assert_eq!(job(0, 0).percent(), 0);
}

#[test]
fn percent_matches_floor_ratio_for_ordinary_counts() {
    assert_eq!(job(1, 3).percent(), 33);
    assert_eq!(job(2, 3).percent(), 66);
    assert_eq!(job(u64::MAX / 100, u64::MAX / 100).percent(), 100);
    assert_eq!(
        job(u64::MAX / 100, u64::MAX / 100 - 1).percent(),
        100,
        "done exceeding total must clamp to 100"
    );
}

#[cfg(not(target_arch = "wasm32"))]
mod proptests {
    use super::job;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn percent_always_stays_within_hundred(done in any::<u64>(), total in any::<u64>()) {
            prop_assert!(job(done, total).percent() <= 100);
        }

        #[test]
        fn percent_matches_reference_ratio(
            done in 0u64..=(u64::MAX / 100),
            total in any::<u64>(),
        ) {
            let expected =
                ((u128::from(done) * 100) / u128::from(total).max(1)).min(100) as u8;
            prop_assert_eq!(job(done, total).percent(), expected);
        }
    }
}
