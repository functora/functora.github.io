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
        let (elegance_theme, preference) = match self {
            Self::Light => (elegance::Theme::frost(), egui::ThemePreference::Light),
            Self::Dark => (elegance::Theme::slate(), egui::ThemePreference::Dark),
        };
        ctx.set_theme(preference);
        elegance_theme.install(ctx);
    }
}
