//! Navigation contracts (plan step 6b): `navigate_to` keeps the sidebar
//! selection and the router in sync, and every catalog entry round-trips
//! through its route string.

use functora_egui_demo::{ShowcaseApp, component_count, component_index, component_name};
use std::str::FromStr as _;

#[test]
fn navigate_to_syncs_selection_and_router() {
    let mut state = ShowcaseApp::default();
    assert_eq!(state.selected, 0);
    let button = component_index("Button").unwrap_or(1);
    state.navigate_to(button);
    assert_eq!(state.selected, button);
    assert_eq!(
        state.router.current().to_flat(),
        Some(button),
        "router must follow the sidebar selection"
    );
    assert_eq!(
        state.router.current().to_string(),
        component_name(button).to_lowercase()
    );
}

#[test]
fn overview_route_roundtrips_through_string() {
    let route = functora_egui_demo::route::AppRoute::from_str("overview");
    assert_eq!(route, Ok(functora_egui_demo::route::AppRoute::Overview));
    let back = functora_egui_demo::route::AppRoute::from_str("OVERVIEW");
    assert_eq!(back, route, "route parsing must be case-insensitive");
    assert!(
        functora_egui_demo::route::AppRoute::from_str("no such component").is_err(),
        "unknown routes must fail"
    );
}

#[test]
fn every_component_navigates_and_reports_its_name() {
    let mut state = ShowcaseApp::default();
    for flat in 0..component_count() {
        state.navigate_to(flat);
        assert_eq!(state.selected, flat);
        assert_eq!(state.router.current().to_flat(), Some(flat));
        let name = component_name(flat);
        assert!(!name.is_empty());
        assert_eq!(component_index(&name.to_lowercase()), Some(flat));
    }
}
