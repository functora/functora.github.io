//! Regression tests for showcase state isolation (plan step 1).
//!
//! Each interactive demo must own its state. Sharing one `String` across two
//! demos leaks edits from one screen into another.

use functora_egui_demo::ShowcaseApp;

#[test]
fn input_group_demos_do_not_share_state() {
    let mut state = ShowcaseApp {
        input_group_url: "example.com".to_owned(),
        input_group_search: "egui".to_owned(),
        ..ShowcaseApp::default()
    };
    assert_eq!(state.input_group_url, "example.com");
    assert_eq!(state.input_group_search, "egui");
    state.input_group_url.push_str("-edited");
    assert_eq!(state.input_group_search, "egui");
}

#[test]
fn email_demos_do_not_share_state() {
    let mut state = ShowcaseApp {
        flex_email: "flex@example.com".to_owned(),
        label_email: "label@example.com".to_owned(),
        field_set_email: "fieldset@example.com".to_owned(),
        ..ShowcaseApp::default()
    };
    assert_eq!(state.flex_email, "flex@example.com");
    assert_eq!(state.label_email, "label@example.com");
    assert_eq!(state.field_set_email, "fieldset@example.com");
    state.flex_email.push_str("-edited");
    assert_eq!(state.label_email, "label@example.com");
    assert_eq!(state.field_set_email, "fieldset@example.com");
}

#[test]
fn password_demos_do_not_share_state() {
    let mut state = ShowcaseApp {
        flex_input: "flex message".to_owned(),
        field_description_password: "s3cret".to_owned(),
        ..ShowcaseApp::default()
    };
    assert_eq!(state.flex_input, "flex message");
    assert_eq!(state.field_description_password, "s3cret");
    state.flex_input.push_str("-edited");
    assert_eq!(state.field_description_password, "s3cret");
}

#[test]
fn blend_demos_do_not_share_state() {
    let mut state = ShowcaseApp {
        select_blend: "Normal".to_owned(),
        property_blend: "Multiply".to_owned(),
        ..ShowcaseApp::default()
    };
    assert_eq!(state.select_blend, "Normal");
    assert_eq!(state.property_blend, "Multiply");
    state.select_blend.push_str("-edited");
    assert_eq!(state.property_blend, "Multiply");
}

#[test]
fn navigation_updates_selection_without_unsafe_aliasing() {
    let mut state = ShowcaseApp::default();
    let target = functora_egui_demo::component_index("Button").unwrap_or(1);
    state.navigate_to(target);
    assert_eq!(state.selected, target);
}
