use std::borrow::Cow;
use std::str::FromStr;

use functora_egui::Routable;
use functora_egui::i18n::Language;
use functora_egui::route::{RouteKind, RouteMetadata};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
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

impl std::fmt::Display for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Home => "home",
            Self::Open => "open",
            Self::View => "view",
            Self::Share => "share",
            Self::About => "about",
            Self::Donate => "donate",
            Self::License => "license",
            Self::Privacy => "privacy",
            Self::File => "file",
        })
    }
}

impl FromStr for Screen {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "home" => Ok(Self::Home),
            "open" => Ok(Self::Open),
            "view" => Ok(Self::View),
            "share" => Ok(Self::Share),
            "about" => Ok(Self::About),
            "donate" => Ok(Self::Donate),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            "file" => Ok(Self::File),
            _ => Err(format!("Unknown screen '{s}'")),
        }
    }
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
        let base = self.to_url();
        if let Some(n) = note {
            if base.contains('?') {
                format!("{base}&note={}", urlencoding::encode(n))
            } else {
                format!("{base}?note={}", urlencoding::encode(n))
            }
        } else {
            base
        }
    }
}
