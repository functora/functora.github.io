//! Catalog invariants for the showcase app: ids, names, counts, and slugs.

use functora_egui_demo::{CATEGORIES, CategoryId, ComponentId, component_count};

fn catalog_names() -> impl Iterator<Item = &'static str> {
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter().map(|def| def.name))
}

#[test]
fn names_are_unique_and_non_empty() {
    let mut seen = std::collections::HashSet::new();
    for name in catalog_names() {
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
fn every_id_matches_exactly_one_catalog_entry() {
    let mut seen = std::collections::HashSet::new();
    for id in ComponentId::ALL {
        assert!(seen.insert(id), "duplicate component id: {id:?}");
        let matches = catalog_names().filter(|name| *name == id.name()).count();
        assert_eq!(matches, 1, "id {id:?} must match exactly one catalog name");
    }
}

#[test]
fn every_catalog_component_has_an_id() {
    let mut covered = 0;
    for (cat_id, _, items) in CATEGORIES {
        for def in *items {
            if *cat_id == CategoryId::Overview {
                assert_eq!(def.id, None, "overview entry must stay id-less");
            } else {
                assert!(
                    def.id.is_some(),
                    "catalog entry {} must have an id",
                    def.name
                );
                covered += 1;
            }
        }
    }
    assert_eq!(covered, ComponentId::ALL.len());
}

#[test]
fn slugs_roundtrip_through_from_slug() {
    for id in ComponentId::ALL {
        assert_eq!(ComponentId::from_slug(&id.slug()), Some(id));
    }
}

#[test]
fn slugs_are_unique_and_lowercase() {
    let mut seen = std::collections::HashSet::new();
    for id in ComponentId::ALL {
        let slug = id.slug();
        assert_eq!(slug, slug.to_ascii_lowercase());
        assert!(seen.insert(slug.clone()), "duplicate slug: {slug}");
    }
}

#[test]
fn from_slug_is_case_insensitive_and_trimmed() {
    assert_eq!(
        ComponentId::from_slug("Button"),
        ComponentId::from_slug("  button ")
    );
    assert_eq!(
        ComponentId::from_slug("Button"),
        ComponentId::from_slug("BUTTON")
    );
    assert_eq!(ComponentId::from_slug("Definitely Not A Component"), None);
    assert_eq!(ComponentId::from_slug(""), None);
}
