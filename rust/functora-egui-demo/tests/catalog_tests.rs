//! Catalog invariants for the showcase app: names, counts, and lookups.

use functora_egui_demo::{
    CATEGORIES, component_count, component_index, component_name, flat_index,
};

fn names() -> impl Iterator<Item = &'static str> {
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter().map(|def| def.name))
}

#[test]
fn names_are_unique_and_non_empty() {
    let mut seen = std::collections::HashSet::new();
    for name in names() {
        assert!(!name.is_empty());
        assert!(seen.insert(name), "duplicate component name: {name}");
    }
}

#[test]
fn count_matches_catalog() {
    let expected: usize = CATEGORIES.iter().map(|(_, _, items)| items.len()).sum();
    assert_eq!(component_count(), expected);
    assert!(component_count() > 0);
}

#[test]
fn flat_index_roundtrips_through_names() {
    for (cat, (_, _, items)) in CATEGORIES.iter().enumerate() {
        for (item, def) in items.iter().enumerate() {
            let flat = flat_index(cat, item);
            assert_eq!(component_name(flat), def.name);
            assert_eq!(component_index(def.name), Some(flat));
        }
    }
}

#[test]
fn every_flat_index_is_named() {
    for flat in 0..component_count() {
        let name = component_name(flat);
        assert!(!name.is_empty());
        assert_eq!(component_index(name), Some(flat));
    }
}

#[test]
fn flat_index_bounds() {
    assert_eq!(flat_index(0, 0), 0);
    let last_cat = CATEGORIES.len() - 1;
    let last_item = CATEGORIES[last_cat].2.len() - 1;
    assert_eq!(flat_index(last_cat, last_item), component_count() - 1);
    assert_eq!(flat_index(1, 0), flat_index(0, CATEGORIES[0].2.len()));
}

#[test]
fn lookups_are_case_insensitive_and_trimmed() {
    let button = component_index("Button");
    assert_eq!(button, component_index("  button "));
    assert_eq!(button, component_index("BUTTON"));
    assert_eq!(component_index("Definitely Not A Component"), None);
    assert_eq!(component_index(""), None);
}

#[test]
fn out_of_range_names_are_empty() {
    assert_eq!(component_name(component_count()), "");
    assert_eq!(component_name(component_count() + 100), "");
}
