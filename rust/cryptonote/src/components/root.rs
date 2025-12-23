use crate::components::*;
use crate::crypto::*;
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Screen {
    #[default]
    Home,
    View,
    Share,
    Donate,
    License,
    Privacy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ActionMode {
    Create,
    Open,
}

impl Display for Screen {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str(match self {
            Self::Home => "home",
            Self::View => "view",
            Self::Share => "share",
            Self::Donate => "donate",
            Self::License => "license",
            Self::Privacy => "privacy",
        })
    }
}

impl FromStr for Screen {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            "view" => Ok(Self::View),
            "share" => Ok(Self::Share),
            "donate" => Ok(Self::Donate),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            _ => Err(format!("Unknown screen '{}'", s)),
        }
    }
}

impl Screen {
    pub(crate) fn to_route(
        &self,
        note: Option<String>,
    ) -> Route {
        Route::Root {
            screen: self.clone(),
            note,
        }
    }
}

#[derive(Clone, Debug)]
pub struct AppCtx {
    pub action: ActionMode,
    pub content: String,
    pub password: String,
    pub cipher: Option<CipherType>,
}

impl Default for AppCtx {
    fn default() -> Self {
        Self {
            action: ActionMode::Create,
            content: String::new(),
            password: String::new(),
            cipher: Some(CipherType::Aes256Gcm),
        }
    }
}

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(Layout)]
        #[route("/?:screen&:note")]
        Root { screen: Screen, note: Option<String> },
}

#[component]
fn Root(screen: Screen, note: Option<String>) -> Element {
    match screen {
        Screen::Home => rsx! {
            Home {}
        },
        Screen::View => rsx! {
            View { note }
        },
        Screen::Share => rsx! {
            Share {}
        },
        Screen::Donate => rsx! {
            Donate {}
        },
        Screen::License => rsx! {
            License {}
        },
        Screen::Privacy => rsx! {
            Privacy {}
        },
    }
}
