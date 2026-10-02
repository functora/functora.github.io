use functora_egui::i18n::Language;
use functora_egui_demo::{CATEGORIES, ComponentId, palette_entries};

#[test]
fn palette_starts_with_ungrouped_overview_button() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let entries = palette_entries(lang);
        let (target, item) = &entries[0];
        assert_eq!(
            *target, None,
            "first palette entry must navigate to Overview"
        );
        assert_eq!(item.label, "Overview");
        assert!(
            item.group.is_empty(),
            "overview must have no group heading, only a clickable button"
        );
    }
}

#[test]
fn palette_covers_every_component_plus_overview() {
    let entries = palette_entries(Language::Eng);
    assert_eq!(entries.len(), ComponentId::ALL.len() + 1);
    for id in ComponentId::ALL {
        assert!(
            entries.iter().any(|(target, _)| *target == Some(id)),
            "palette must contain {id:?}"
        );
    }
}

#[test]
fn palette_groups_follow_catalog_order() {
    let entries = palette_entries(Language::Eng);
    let groups: Vec<String> = entries.iter().map(|(_, item)| item.group.clone()).collect();
    let mut seen: Vec<String> = Vec::new();
    for group in groups {
        if seen.last() != Some(&group) {
            seen.push(group);
        }
    }
    let expected: Vec<String> = std::iter::once(String::new())
        .chain(
            CATEGORIES
                .iter()
                .skip(1)
                .map(|(cat, _, _)| cat.label().to_owned()),
        )
        .collect();
    assert_eq!(seen, expected, "palette groups must follow catalog order");
    assert!(
        seen.contains(&"Inputs".to_owned()),
        "Inputs group must be present in the palette"
    );
}

#[test]
fn overview_roundtrip_through_navigate_to() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    state.navigate_to(Some(ComponentId::Button));
    assert_eq!(state.selected, Some(ComponentId::Button));
    state.navigate_to(None);
    assert_eq!(state.selected, None);
    assert_eq!(
        state.router.current().clone(),
        functora_egui_demo::route::AppRoute::Overview
    );
}
