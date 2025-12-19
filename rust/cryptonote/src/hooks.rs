use crate::components::{NavigationState, Route};
use crate::prelude::*;
use dioxus_router::Navigator;

#[derive(Clone, Copy)]
pub struct AppNavigator {
    nav: Navigator,
    state: Signal<NavigationState>,
}

impl AppNavigator {
    pub fn push(mut self, route: Route) {
        self.state.write().has_navigated = true;
        self.nav.push(route);
    }

    pub fn go_back(self) {
        self.nav.go_back();
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
    let state = use_context::<Signal<NavigationState>>();
    AppNavigator { nav, state }
}
