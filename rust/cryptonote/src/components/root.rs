use crate::components::*;
use crate::crypto::*;
use crate::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Screen {
    Home,
    View,
    Share,
    Donate,
    License,
    Privacy,
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "home" => Ok(Self::Home),
            "view" => Ok(Self::View),
            "share" => Ok(Self::Share),
            "donate" => Ok(Self::Donate),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            _ => Err(()),
        }
    }
}

impl Screen {
    pub(crate) fn to_route(
        &self,
        note: Option<String>,
    ) -> Route {
        Route::Root {
            screen: Some(self.to_string()),
            note,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct AppContext {
    pub content: Option<String>,
    pub password: String,
    pub cipher: Option<CipherType>,
    pub share_url: Option<String>,
    pub qr_code: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct NavigationState {
    pub has_navigated: bool,
}

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(Layout)]
        #[route("/?:screen&:note")]
        Root { screen: Option<String>, note: Option<String> },
}

#[component]
fn Root(
    screen: Option<String>,
    note: Option<String>,
) -> Element {
    let parsed_screen = screen
        .as_deref()
        .and_then(|s| s.parse::<Screen>().ok())
        .unwrap_or(Screen::Home);

    match parsed_screen {
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
