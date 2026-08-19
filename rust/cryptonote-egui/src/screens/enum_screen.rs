use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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

impl Display for Screen {
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
            _ => Err(format!("Unknown screen '{s}'")),
        }
    }
}
