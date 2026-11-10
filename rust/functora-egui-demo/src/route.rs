use functora_egui::i18n::{I18N, Language};
use functora_egui::route::{RouteKind, RouteMetadata};
use std::borrow::Cow;

use crate::catalog::{CATEGORIES, CategoryId, ComponentId};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum AppRoute {
    #[default]
    Overview,
    Component(ComponentId),
}

impl std::fmt::Display for AppRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Overview => write!(f, "overview"),
            Self::Component(id) => write!(f, "{}", id.slug()),
        }
    }
}

impl std::str::FromStr for AppRoute {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("overview") {
            Ok(Self::Overview)
        } else if let Some(id) = ComponentId::from_slug(s) {
            Ok(Self::Component(id))
        } else {
            Err(format!("unknown route: {s}"))
        }
    }
}

impl RouteMetadata for AppRoute {
    fn label(&self, lang: Language) -> Cow<'static, str> {
        match self {
            Self::Overview => CategoryId::Overview.render(lang).into(),
            Self::Component(id) => id.name().into(),
        }
    }

    fn parent(&self) -> Option<Self> {
        match self {
            Self::Component(_) => Some(Self::Overview),
            Self::Overview => None,
        }
    }

    fn children(&self) -> Vec<Self> {
        match self {
            Self::Overview => ComponentId::ALL.map(Self::Component).to_vec(),
            Self::Component(_) => vec![],
        }
    }

    fn kind(&self) -> RouteKind {
        RouteKind::Page
    }
}

impl AppRoute {
    #[must_use]
    pub fn component(&self) -> Option<ComponentId> {
        match self {
            Self::Overview => None,
            Self::Component(id) => Some(*id),
        }
    }

    /// The screens before and after this one in catalog order.
    ///
    /// The order is the one the sidebar, the command palette and the overview
    /// share: the overview first, then every component in category order. A
    /// missing neighbour is `None`, so the first and the last screen have one
    /// side only.
    #[must_use]
    pub fn neighbors(&self) -> (Option<Self>, Option<Self>) {
        let routes = || {
            CATEGORIES
                .iter()
                .flat_map(|(_, _, items)| items.iter())
                .map(|def| def.id.map_or(Self::Overview, Self::Component))
        };
        let index = routes().position(|route| route == *self);
        (
            index
                .and_then(|at| at.checked_sub(1))
                .and_then(|at| routes().nth(at)),
            index
                .and_then(|at| at.checked_add(1))
                .and_then(|at| routes().nth(at)),
        )
    }
}
