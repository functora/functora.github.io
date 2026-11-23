use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

use functora_egui::i18n::Language;
use functora_egui::route::RouteMetadata;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Screen {
    #[default]
    Home,
    Chat,
    Security,
    About,
    License,
    Privacy,
}

impl fmt::Display for Screen {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Home => "home",
            Self::Chat => "chat",
            Self::Security => "security",
            Self::About => "about",
            Self::License => "license",
            Self::Privacy => "privacy",
        })
    }
}

impl FromStr for Screen {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.to_ascii_lowercase().as_str() {
            "home" => Ok(Self::Home),
            "chat" => Ok(Self::Chat),
            "security" => Ok(Self::Security),
            "about" => Ok(Self::About),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            _ => Err(format!("unknown screen: {value}")),
        }
    }
}

impl RouteMetadata for Screen {
    fn label(&self, _language: Language) -> Cow<'static, str> {
        Cow::Borrowed(match self {
            Self::Home => "Home",
            Self::Chat => "Conversations",
            Self::Security => "Security",
            Self::About => "About",
            Self::License => "License",
            Self::Privacy => "Privacy",
        })
    }

    fn parent(&self) -> Option<Self> {
        None
    }

    fn children(&self) -> Vec<Self> {
        Vec::new()
    }
}
