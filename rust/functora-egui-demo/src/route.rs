use functora_egui::i18n::{I18N, Language};
use functora_egui::route::{RouteKind, RouteMetadata};
use std::borrow::Cow;

use crate::app::{CATEGORIES, CategoryId, component_index, component_name};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum AppRoute {
    #[default]
    Overview,
    Component(usize),
}

impl std::fmt::Display for AppRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Overview => write!(f, "overview"),
            Self::Component(idx) => write!(f, "{}", component_name(*idx).to_lowercase()),
        }
    }
}

impl std::str::FromStr for AppRoute {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("overview") {
            Ok(Self::Overview)
        } else if let Some(idx) = component_index(s) {
            Ok(Self::Component(idx))
        } else {
            Err(format!("unknown route: {s}"))
        }
    }
}

impl RouteMetadata for AppRoute {
    fn label(&self, lang: Language) -> Cow<'static, str> {
        match self {
            Self::Overview => CategoryId::Overview.render(lang).into(),
            Self::Component(idx) => component_name(*idx).into(),
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
            Self::Overview => {
                let mut idx = 0;
                CATEGORIES
                    .iter()
                    .flat_map(|(_, _, items)| {
                        items.iter().map(move |_| {
                            let result = Self::Component(idx);
                            idx += 1;
                            result
                        })
                    })
                    .collect()
            }
            Self::Component(_) => vec![],
        }
    }

    fn kind(&self) -> RouteKind {
        RouteKind::Page
    }
}

impl AppRoute {
    #[must_use]
    pub fn from_flat(idx: usize) -> Self {
        if idx == 0 {
            Self::Overview
        } else {
            Self::Component(idx)
        }
    }
    #[must_use]
    pub fn to_flat(&self) -> Option<usize> {
        match self {
            Self::Overview => Some(0),
            Self::Component(idx) => Some(*idx),
        }
    }
}
