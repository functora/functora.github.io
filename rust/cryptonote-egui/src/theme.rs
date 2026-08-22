use functora_egui::theme::{shadcn_theme_dark, shadcn_theme_light};
use functora_egui::ShadcnThemeExt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Theme {
    Light,
    #[default]
    Dark,
}

impl Theme {
    #[must_use]
    pub const fn toggle(self) -> Self {
        match self {
            Self::Light => Self::Dark,
            Self::Dark => Self::Light,
        }
    }

    pub fn apply(self, ctx: &egui::Context) {
        let (shadcn_theme, preference) = match self {
            Self::Light => (shadcn_theme_light::light(), egui::ThemePreference::Light),
            Self::Dark => (shadcn_theme_dark::dark(), egui::ThemePreference::Dark),
        };
        ctx.set_theme(preference);
        ctx.set_shadcn_theme(shadcn_theme);
    }
}
