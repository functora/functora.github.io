//! Navigation contracts (plan step 6b): `navigate_to` keeps the sidebar
//! selection and the router in sync, and every catalog entry round-trips
//! through its route string.

use functora_egui_demo::{ComponentId, ShowcaseApp};
use std::str::FromStr as _;

#[test]
fn navigate_to_syncs_selection_and_router() {
    let mut state = ShowcaseApp::default();
    assert_eq!(state.selected, None);
    state.navigate_to(Some(ComponentId::Button));
    assert_eq!(state.selected, Some(ComponentId::Button));
    assert_eq!(
        state.router.current().component(),
        Some(ComponentId::Button),
        "router must follow the sidebar selection"
    );
    assert_eq!(
        state.router.current().to_string(),
        ComponentId::Button.slug()
    );
}

#[test]
fn navigate_home_clears_selection() {
    let mut state = ShowcaseApp::default();
    state.navigate_to(Some(ComponentId::Button));
    state.navigate_to(None);
    assert_eq!(state.selected, None);
    assert_eq!(
        state.router.current().clone(),
        functora_egui_demo::route::AppRoute::Overview
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
    for id in ComponentId::ALL {
        state.navigate_to(Some(id));
        assert_eq!(state.selected, Some(id));
        assert_eq!(state.router.current().component(), Some(id));
        assert!(!id.name().is_empty());
        assert_eq!(ComponentId::from_slug(&id.slug()), Some(id));
        assert_eq!(ComponentId::from_slug(&id.name().to_lowercase()), Some(id));
    }
}
