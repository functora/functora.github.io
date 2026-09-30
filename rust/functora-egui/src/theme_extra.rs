use serde::{Deserialize, Serialize};

fn theme_key() -> egui::Id {
    egui::Id::new("functora_theme")
}

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
    ctx.data_mut(|d| {
        let _ = d.insert_temp(theme_key(), theme);
    });
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
    ctx.data(|d| d.get_temp::<Theme>(theme_key()).unwrap_or(Theme::Light))
}

#[must_use]
pub fn detect_system_theme(ctx: &egui::Context) -> Option<Theme> {
    ctx.system_theme().map(|t| match t {
        egui::Theme::Light => Theme::Light,
        egui::Theme::Dark => Theme::Dark,
    })
}

#[cfg(all(target_arch = "wasm32", feature = "platform"))]
#[must_use]
pub fn detect_system_theme_wasm() -> Option<Theme> {
    web_sys::window()
        .and_then(|w| w.match_media("(prefers-color-scheme: dark)").ok()?)
        .map(|m| {
            if m.matches() {
                Theme::Dark
            } else {
                Theme::Light
            }
        })
}

#[cfg(not(all(target_arch = "wasm32", feature = "platform")))]
#[must_use]
pub fn detect_system_theme_wasm() -> Option<Theme> {
    None
}

#[must_use]
pub fn default_theme(ctx: &egui::Context) -> Theme {
    detect_system_theme(ctx)
        .or_else(detect_system_theme_wasm)
        .unwrap_or(Theme::Light)
}
