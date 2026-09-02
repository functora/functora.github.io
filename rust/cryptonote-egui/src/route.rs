use std::borrow::Cow;

use functora_egui::Routable;
use functora_egui::i18n::Language;
use functora_egui::route::{RouteKind, RouteMetadata};
use strum::{Display, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Display, EnumString)]
#[strum(serialize_all = "lowercase", ascii_case_insensitive)]
pub enum Screen {
    #[default]
    Home,
    Open,
    View,
    Share,
    About,
    Donate,
    License,
    Privacy,
    File,
}

impl RouteMetadata for Screen {
    fn label(&self, _lang: Language) -> Cow<'static, str> {
        match self {
            Self::Home => "Home".into(),
            Self::Open => "Open".into(),
            Self::View => "View".into(),
            Self::Share => "Share".into(),
            Self::About => "About".into(),
            Self::Donate => "Donate".into(),
            Self::License => "License".into(),
            Self::Privacy => "Privacy".into(),
            Self::File => "File".into(),
        }
    }

    fn parent(&self) -> Option<Self> {
        match self {
            Self::Home => None,
            Self::File => Some(Self::View),
            Self::Open | Self::View | Self::Share | Self::About | Self::Donate | Self::License | Self::Privacy => {
                Some(Self::Home)
            }
        }
    }

    fn children(&self) -> Vec<Self> {
        match self {
            Self::Home => vec![
                Self::Open,
                Self::View,
                Self::Share,
                Self::File,
                Self::About,
                Self::Donate,
                Self::License,
                Self::Privacy,
            ],
            _ => vec![],
        }
    }

    fn kind(&self) -> RouteKind {
        RouteKind::Page
    }
}

impl Screen {
    #[must_use]
    pub fn to_url_with_note(&self, note: Option<&str>) -> String {
        note.map_or_else(
            || self.to_url(),
            |n| functora_core::encoding::append_query_param(&self.to_url(), crate::encoding::NOTE_PARAM, n),
        )
    }
}
