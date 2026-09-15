use functora_egui::i18n::{I18N, Language};
use functora_egui::route::{RouteKind, RouteMetadata};
use std::borrow::Cow;

use crate::app::{CategoryId, ComponentId};

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
}
