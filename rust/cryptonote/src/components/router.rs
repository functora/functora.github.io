use crate::components::screens::*;
use crate::*;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Screen {
    #[default]
    Home,
    View,
    Share,
    About,
    Donate,
    License,
    Privacy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ActionMode {
    Create,
    Open,
    Scan,
}

impl std::fmt::Display for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Home => "home",
            Self::View => "view",
            Self::Share => "share",
            Self::About => "about",
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
            "about" => Ok(Self::About),
            "donate" => Ok(Self::Donate),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            _ => Err(format!("Unknown screen '{}'", s)),
        }
    }
}

impl Screen {
    pub(crate) fn to_route(&self, note: Option<String>) -> Route {
        Route::Root {
            screen: self.clone(),
            note,
        }
    }
}

#[derive(Store, Default)]
pub struct HomeState {
    pub url_input: String,
}

#[derive(Store, Default)]
pub struct ViewState {
    pub note_content: Option<String>,
    pub encrypted_data: Option<EncryptedData>,
    pub password_input: String,
    pub is_encrypted: bool,
}

#[derive(Store)]
pub struct TemporaryState {
    pub action: ActionMode,
    pub content: String,
    pub password: String,
    pub cipher: Option<CipherType>,
    pub home: HomeState,
    pub view: ViewState,
}

impl Default for TemporaryState {
    fn default() -> Self {
        Self {
            action: ActionMode::Create,
            content: String::new(),
            password: String::new(),
            cipher: Some(CipherType::Aes256Gcm),
            home: HomeState::default(),
            view: ViewState::default(),
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

impl Default for Route {
    fn default() -> Self {
        Self::Root {
            screen: Screen::default(),
            note: None,
        }
    }
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
        Screen::About => rsx! {
            About {}
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
