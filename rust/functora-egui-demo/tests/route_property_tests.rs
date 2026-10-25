//! Catalog routes and preview truncation must stay bijective and never split
//! a codepoint, for arbitrary inputs.
#![cfg(not(target_arch = "wasm32"))]

use functora_egui_demo::route::AppRoute;
use functora_egui_demo::{ComponentId, ShowcaseApp};
use proptest::prelude::*;
use proptest::test_runner::RngSeed;
use std::str::FromStr;

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn component_slug_roundtrips(index in 0..ComponentId::ALL.len()) {
        let id = ComponentId::ALL[index];
        let slug = id.slug();
        let parsed = ComponentId::from_slug(&slug)
            .unwrap_or_else(|| panic!("slug {slug:?} must parse back"));
        prop_assert_eq!(parsed, id);
    }

    #[test]
    fn from_slug_tolerates_case_and_whitespace(
        index in 0..ComponentId::ALL.len(),
        pad in "[ ]{0,4}",
    ) {
        let id = ComponentId::ALL[index];
        let noisy = format!("{pad}{}{pad}", id.slug().to_ascii_uppercase());
        let parsed = ComponentId::from_slug(&noisy)
            .unwrap_or_else(|| panic!("noisy slug {noisy:?} must parse back"));
        prop_assert_eq!(parsed, id);
    }

    #[test]
    fn app_route_display_from_str_roundtrips(index in 0..ComponentId::ALL.len()) {
        let route = AppRoute::Component(ComponentId::ALL[index]);
        let rendered = route.to_string();
        let parsed = AppRoute::from_str(&rendered)
            .unwrap_or_else(|err| panic!("route {rendered:?} must parse back: {err}"));
        prop_assert_eq!(parsed, route);
    }

    #[test]
    fn truncate_preview_url_is_prefix_safe(url in ".*", limit in 0..256_usize) {
        let shown = ShowcaseApp::truncate_preview_url(&url, limit);
        prop_assert!(url.starts_with(shown), "result must be a prefix");
        prop_assert!(shown.len() <= limit, "result must respect the byte limit");
    }

    #[test]
    fn truncate_preview_url_never_splits_a_codepoint(
        head in "[a-z]{0,16}",
        body in "[é🎉一]{0,32}",
        limit in 0..128_usize,
    ) {
        let url = format!("bytes://{head}{body}");
        let shown = ShowcaseApp::truncate_preview_url(&url, limit);
        prop_assert!(url.starts_with(shown), "result must be a prefix");
        prop_assert!(shown.len() <= limit, "result must respect the byte limit");
        prop_assert!(shown.is_char_boundary(shown.len()), "must not split a codepoint");
    }
}

#[test]
fn overview_route_roundtrips() {
    let rendered = AppRoute::Overview.to_string();
    match AppRoute::from_str(&rendered) {
        Ok(route) => assert_eq!(route, AppRoute::Overview),
        Err(err) => panic!("route {rendered:?} must parse back: {err}"),
    }
}
