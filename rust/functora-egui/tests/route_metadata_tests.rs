//! `RouteMetadata`, `breadcrumbs_for` and URL round-trips, including the
//! Category-skipping and Modal/External kinds.

use functora_egui::i18n::Language;
use functora_egui::route::{
    BreadcrumbPosition, BreadcrumbSegment, Routable, RouteKind, RouteMetadata, breadcrumbs_for,
};
use std::borrow::Cow;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
enum TestRoute {
    #[default]
    Home,
    Docs,
    Section,
    Dialog,
    External,
}

impl Display for TestRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Home => write!(f, "home"),
            Self::Docs => write!(f, "docs"),
            Self::Section => write!(f, "section"),
            Self::Dialog => write!(f, "dialog"),
            Self::External => write!(f, "external"),
        }
    }
}

impl FromStr for TestRoute {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            "docs" => Ok(Self::Docs),
            "section" => Ok(Self::Section),
            "dialog" => Ok(Self::Dialog),
            "external" => Ok(Self::External),
            _ => Err(format!("unknown route: {s}")),
        }
    }
}

impl RouteMetadata for TestRoute {
    fn label(&self, _lang: Language) -> Cow<'static, str> {
        match self {
            Self::Home => "Home".into(),
            Self::Docs => "Docs".into(),
            Self::Section => "Section".into(),
            Self::Dialog => "Dialog".into(),
            Self::External => "External".into(),
        }
    }

    fn parent(&self) -> Option<Self> {
        match self {
            Self::Home => None,
            Self::Dialog => Some(Self::Docs),
            Self::Docs | Self::Section | Self::External => Some(Self::Home),
        }
    }

    fn children(&self) -> Vec<Self> {
        match self {
            Self::Home => vec![Self::Docs, Self::Section, Self::External],
            Self::Docs => vec![Self::Dialog],
            Self::Section | Self::Dialog | Self::External => vec![],
        }
    }

    fn kind(&self) -> RouteKind {
        match self {
            Self::Section => RouteKind::Category,
            Self::Dialog => RouteKind::Modal,
            Self::External => RouteKind::External,
            Self::Home | Self::Docs => RouteKind::Page,
        }
    }
}

#[test]
fn breadcrumbs_skip_category_and_mark_last() {
    let chain = breadcrumbs_for(&TestRoute::Dialog, Language::Eng);
    let names: Vec<_> = chain.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(names, vec!["Home", "Docs", "Dialog"]);
    assert!(chain.iter().take(2).all(|s| !s.is_last()));
    assert!(chain.last().is_some_and(BreadcrumbSegment::is_last));
    assert!(
        chain
            .last()
            .is_some_and(|s| s.position == BreadcrumbPosition::Last)
    );
}

#[test]
fn breadcrumbs_cover_all_kinds() {
    for route in [
        TestRoute::Home,
        TestRoute::Docs,
        TestRoute::Dialog,
        TestRoute::External,
    ] {
        let chain = breadcrumbs_for(&route, Language::Eng);
        assert!(!chain.is_empty(), "{route:?} must produce breadcrumbs");
        assert!(chain.last().is_some_and(BreadcrumbSegment::is_last));
    }
    let section = breadcrumbs_for(&TestRoute::Section, Language::Eng);
    assert_eq!(
        section.iter().map(|s| s.name.as_str()).collect::<Vec<_>>(),
        vec!["Home"]
    );
}

#[test]
fn url_roundtrip_preserves_routes() {
    for route in [TestRoute::Docs, TestRoute::Dialog, TestRoute::External] {
        let url = route.to_url();
        assert_eq!(
            TestRoute::from_url(&url),
            Some(route),
            "url round-trip failed for {route:?}"
        );
    }
}

#[test]
fn default_route_maps_to_root() {
    assert_eq!(TestRoute::Home.to_url(), "/");
    assert_eq!(TestRoute::from_url("/"), None);
}
