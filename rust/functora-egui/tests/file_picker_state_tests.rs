#![cfg(feature = "files")]
#![cfg(not(target_arch = "wasm32"))]

use functora_egui::files::FilePickerState;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

#[test]
fn known_picker_codes_map_to_their_states() {
    assert_eq!(FilePickerState::from(0), FilePickerState::Pending);
    assert_eq!(FilePickerState::from(1), FilePickerState::Done);
    assert_eq!(FilePickerState::from(-1), FilePickerState::Cancelled);
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn picker_state_mapping_is_total_and_preserves_unknown_codes(code in any::<i32>()) {
        let preserves_code = match FilePickerState::from(code) {
            FilePickerState::Pending => code == 0,
            FilePickerState::Done => code == 1,
            FilePickerState::Cancelled => code == -1,
            FilePickerState::Failed(reported) => {
                reported == code && code != 0 && code != 1 && code != -1
            }
        };
        prop_assert!(preserves_code);
    }
}
