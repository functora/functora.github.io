//! The donate screen, its route and the links that open it: the footer and
//! the home page must both reach it, and the screen must render the shared
//! white-label donate content.

use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg;
use functora_egui::route::RouteMetadata;
use functora_egui_demo::route::AppRoute;
use functora_egui_demo::{ComponentId, DONATE_ACTION, ShowcaseApp};
use std::str::FromStr;

mod common;
use common::{rendered_text, rendered_text_sized};

#[test]
fn donate_route_roundtrips_and_carries_the_localized_label() {
    assert_eq!(AppRoute::Donate.to_string(), "donate");
    assert_eq!(AppRoute::from_str("donate"), Ok(AppRoute::Donate));
    assert_eq!(AppRoute::from_str("DONATE"), Ok(AppRoute::Donate));
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_eq!(
            AppRoute::Donate.label(lang),
            Msg::Donate.render(lang),
            "the donate route must be labelled in {lang:?}"
        );
    }
}

#[test]
fn the_donate_action_opens_the_donate_screen() {
    assert_eq!(
        ShowcaseApp::route_for_action(DONATE_ACTION),
        Some(AppRoute::Donate)
    );
    assert_eq!(ShowcaseApp::route_for_action("license"), None);
}

#[test]
fn navigating_to_the_donate_screen_clears_the_component_selection() {
    let mut app = ShowcaseApp::default();
    app.navigate_to(Some(ComponentId::Button));
    app.navigate_route(AppRoute::Donate);
    assert_eq!(app.selected, None);
    assert_eq!(app.router.current().clone(), AppRoute::Donate);
}

#[test]
fn the_donate_screen_renders_its_title() {
    let mut app = ShowcaseApp::default();
    app.navigate_route(AppRoute::Donate);
    let text = rendered_text(|ui| {
        _ = app.render_component(ui, Language::Eng);
    });
    assert!(
        text.contains(&Msg::Donate.render(Language::Eng)),
        "the donate screen must carry its title: {text}"
    );
}

#[test]
fn the_donate_screen_renders_the_shared_content() {
    let mut app = ShowcaseApp::default();
    app.navigate_route(AppRoute::Donate);
    let text = rendered_text_sized(
        &mut |ui| {
            _ = app.render_component(ui, Language::Eng);
        },
        4000.0,
    );
    assert!(
        text.contains(&Msg::DonateGreeting.render(Language::Eng)),
        "the donate screen must greet the user: {text}"
    );
    assert!(
        text.contains("Functora"),
        "the donate intro must name the author: {text}"
    );
    for block in ["BTC - Bitcoin", "XMR - Monero"] {
        assert!(
            text.contains(block),
            "the donate screen must list the {block} block: {text}"
        );
    }
}

#[test]
fn the_donate_screen_follows_the_app_language() {
    let mut app = ShowcaseApp::default();
    app.navigate_route(AppRoute::Donate);
    let text = rendered_text_sized(
        &mut |ui| {
            _ = app.render_component(ui, Language::Spa);
        },
        4000.0,
    );
    assert!(
        text.contains(&Msg::DonateGreeting.render(Language::Spa)),
        "the donate screen must greet the user in Spanish: {text}"
    );
    assert!(
        text.contains(&Msg::Donate.render(Language::Spa)),
        "the donate screen must carry its Spanish title: {text}"
    );
}
