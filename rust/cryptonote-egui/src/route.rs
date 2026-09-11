use std::borrow::Cow;

use functora_egui::Routable;
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::route::{RouteKind, RouteMetadata};
use strum::{Display, EnumString};

use crate::messages::Msg;

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
    fn label(&self, lang: Language) -> Cow<'static, str> {
        match self {
            Self::Home => BaseMsg::Home.render(lang).into(),
            Self::Open => Msg::OpenButton.render(lang).into(),
            Self::View => Msg::ViewButton.render(lang).into(),
            Self::Share => Msg::Share.render(lang).into(),
            Self::About => BaseMsg::Application.render(lang).into(),
            Self::Donate => BaseMsg::Donate.render(lang).into(),
            Self::License => BaseMsg::TermsOfServiceTitle.render(lang).into(),
            Self::Privacy => BaseMsg::PrivacyPolicyTitle.render(lang).into(),
            Self::File => Msg::File.render(lang).into(),
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
