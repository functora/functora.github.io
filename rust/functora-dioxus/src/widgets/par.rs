use dioxus::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Align {
    Left,
    Center,
    Right,
    Justify,
}

impl Align {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Left => "l",
            Self::Center => "c",
            Self::Right => "r",
            Self::Justify => "j",
        }
    }
}

#[component]
pub fn Par(
    children: Element,
    #[props(default)] align: Option<Align>,
    #[props(extends = p, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        p {
            "txt": align.map(Align::as_str),
            ..attributes,
            {children}
        }
    }
}
