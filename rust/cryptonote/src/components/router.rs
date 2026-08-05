use crate::components::screens::*;
use crate::*;

#[derive(Debug, Clone, PartialEq, Default)]
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
        match s {
            "home" => Ok(Self::Home),
            "open" => Ok(Self::Open),
            "view" => Ok(Self::View),
            "share" => Ok(Self::Share),
            "about" => Ok(Self::About),
            "donate" => Ok(Self::Donate),
            "license" => Ok(Self::License),
            "privacy" => Ok(Self::Privacy),
            "file" => Ok(Self::File),
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

#[derive(Debug, Clone)]
pub struct ExternalNote {
    pub data: NoteData,
    pub url: String,
    pub qr: String,
}

#[derive(Debug, Clone, Default)]
pub enum External {
    #[default]
    Nothing,
    Note(ExternalNote),
    Archive(ExternalArchive),
}

impl External {
    pub(crate) fn note_url(self) -> String {
        match self {
            Self::Note(n) => n.url,
            _ => String::new(),
        }
    }

    pub(crate) fn archive_bytes(self) -> Vec<u8> {
        match self {
            Self::Archive(a) => a.untag(),
            _ => Vec::new(),
        }
    }
}

#[derive(Store)]
pub struct TemporaryState {
    pub note: String,
    pub password: String,
    pub cipher: Option<CipherType>,
    pub attachments: Vec<Attachment>,
    pub screen: Screen,
    pub action: ActionMode,
    pub url_input: String,
    pub external: External,
    pub progress: Option<Job>,
    pub attachment: Option<usize>,
}

impl Default for TemporaryState {
    fn default() -> Self {
        Self {
            note: String::new(),
            password: String::new(),
            cipher: Some(CipherType::Aes256Gcm),
            attachments: Vec::new(),
            screen: Screen::default(),
            action: ActionMode::Create,
            url_input: String::new(),
            external: External::Nothing,
            progress: None,
            attachment: None,
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
        Screen::Open => rsx! {
            Open { note }
        },
        Screen::View => rsx! {
            View {}
        },
        Screen::Share => rsx! {
            Share {}
        },
        Screen::About => rsx! {
            About { note }
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
        Screen::File => rsx! {
            File {}
        },
    }
}
