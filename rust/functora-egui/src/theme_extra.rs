use serde::{Deserialize, Serialize};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Theme {
    Light,
    Dark,
}

impl Theme {
    #[must_use]
    pub fn next(self) -> Self {
        match self {
            Self::Light => Self::Dark,
            Self::Dark => Self::Light,
        }
    }

    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Light => "light",
            Self::Dark => "dark",
        }
    }
}

impl std::fmt::Display for Theme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Light => write!(f, "Light"),
            Self::Dark => write!(f, "Dark"),
        }
    }
}

pub fn set_theme(ctx: &egui::Context, theme: Theme) {
    match theme {
        Theme::Light => {
            let light = crate::theme::shadcn_theme_light::light();
            crate::theme::shadcn_theme_ext::ShadcnThemeExt::set_shadcn_theme(ctx, light);
        }
        Theme::Dark => {
            let dark = crate::theme::shadcn_theme_dark::dark();
            crate::theme::shadcn_theme_ext::ShadcnThemeExt::set_shadcn_theme(ctx, dark);
        }
    }
}

#[must_use]
pub fn current_theme(ctx: &egui::Context) -> Theme {
    let current = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
    let dark_bg = crate::theme::shadcn_theme_dark::dark().background;
    if current.background == dark_bg {
        Theme::Dark
    } else {
        Theme::Light
    }
}
