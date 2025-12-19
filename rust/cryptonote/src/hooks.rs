use crate::components::Route;
use crate::prelude::*;
use dioxus_router::Navigator;

#[derive(Clone, Copy)]
pub struct AppNavigator {
    nav: Navigator,
    has_navigated: Signal<bool>,
}

impl AppNavigator {
    pub fn push(mut self, route: Route) {
        self.has_navigated.set(true);
        self.nav.push(route);
    }

    pub fn go_back(self) {
        self.nav.go_back();
    }

    pub fn has_havigated(self) -> bool {
        (self.has_navigated)()
    }

    pub fn link(
        self,
        route: Route,
    ) -> impl Fn(Event<MouseData>) {
        move |evt: Event<MouseData>| {
            evt.prevent_default();
            self.push(route.clone());
        }
    }
}

pub fn use_app_navigator() -> AppNavigator {
    let nav = use_navigator();
    let has_navigated = use_context::<Signal<bool>>();
    AppNavigator { nav, has_navigated }
}
