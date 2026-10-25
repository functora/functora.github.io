#![cfg(feature = "platform")]
#![cfg(not(target_arch = "wasm32"))]

use functora_egui::camera::CameraStartStatus;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

#[test]
fn documented_camera_codes_map_to_their_status() {
    assert_eq!(CameraStartStatus::from(1), CameraStartStatus::Running);
    assert_eq!(CameraStartStatus::from(0), CameraStartStatus::Pending);
    assert_eq!(CameraStartStatus::from(-1), CameraStartStatus::Failed);
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn camera_status_mapping_is_total(code in any::<i32>()) {
        let settled_or_pending = match CameraStartStatus::from(code) {
            CameraStartStatus::Running => code == 1,
            CameraStartStatus::Failed => code == -1,
            CameraStartStatus::Pending => code != 1 && code != -1,
        };
        prop_assert!(settled_or_pending);
    }
}
