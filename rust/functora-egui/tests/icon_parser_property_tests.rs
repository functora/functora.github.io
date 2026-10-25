//! SVG path and element parsers must handle arbitrary input without
//! panicking, and must parse well-formed path data into the expected
//! command count.
#![cfg(not(target_arch = "wasm32"))]

use functora_egui::icons::parse_path::parse_path_data;
use functora_egui::icons::parse_svg::parse_svg;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn path_parser_never_panics_on_arbitrary_input(input in ".{0,128}") {
        prop_assert!(parse_path_data(&input).len() <= input.len());
    }

    #[test]
    fn svg_parser_never_panics_on_arbitrary_input(input in ".{0,128}") {
        prop_assert!(parse_svg(&input).len() <= input.len());
    }

    #[test]
    fn close_following_move_parses_two_commands(x in -1.0e6_f32..1.0e6, y in -1.0e6_f32..1.0e6) {
        let data = format!("M {x} {y} Z");
        prop_assert_eq!(parse_path_data(&data).len(), 2);
    }

    #[test]
    fn path_parser_rejects_zero_coordinate_precision(steps in 0..8_usize) {
        // Repeated coordinate pairs after an implicit command must consume
        // exactly `steps` pairs, so the command count grows linearly.
        let mut data = String::from("M 0 0");
        for _ in 0..steps {
            data.push_str(" 1 1");
        }
        prop_assert_eq!(parse_path_data(&data).len(), 1 + steps);
    }
}
