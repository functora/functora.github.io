use crate::components::*;
use crate::i18n::{get_translations, Translations};
use crate::prelude::*;
use crate::storage::AppCfg;
use dioxus_router::Navigator;

#[derive(Clone, Copy)]
pub struct AppNav {
    nav: Navigator,
    idx: Signal<u32>,
}

impl PartialEq for AppNav {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl AppNav {
    pub fn push(mut self, route: Route) {
        let Route::Root { screen, .. } = route.clone();
        let next = if screen == Screen::Home {
            0
        } else {
            (self.idx)().saturating_add(1)
        };
        self.idx.set(next);
        self.nav.push(route);
    }

    pub fn go_back(mut self) {
        if self.nav.can_go_back() {
            let next = (self.idx)().saturating_sub(1);
            self.idx.set(next);
            self.nav.go_back();
        } else {
            self.idx.set(0);
        }
    }

    pub fn has_navigated(self) -> bool {
        (self.idx)() > 0
    }
}

impl functora_dioxus::traits::NavCtx for AppNav {
    fn push_route(&mut self, href: String) {
        if let Ok(route) = href.parse::<Route>() {
            self.idx.set((self.idx)().saturating_add(1));
            self.nav.push(route);
        }
    }

    fn can_go_back(&self) -> bool {
        self.nav.can_go_back()
    }

    fn go_back(&mut self) {
        if self.nav.can_go_back() {
            let next = (self.idx)().saturating_sub(1);
            self.idx.set(next);
            self.nav.go_back();
        } else {
            self.idx.set(0);
        }
    }
}

pub fn use_app_nav() -> AppNav {
    let nav = use_navigator();
    let idx = use_context::<Signal<u32>>();
    AppNav { nav, idx }
}

pub fn use_translations() -> Translations {
    get_translations(
        use_context::<Signal<AppCfg>>().read().language,
    )
}
