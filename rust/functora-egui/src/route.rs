use std::borrow::Cow;
use std::fmt::Display;
use std::str::FromStr;

use functora_core::i18n::Language;

pub trait Routable:
    Display + FromStr + Clone + PartialEq + Eq + std::fmt::Debug + Send + Sync + 'static
{
    #[allow(clippy::must_use_candidate)]
    fn screen_param() -> &'static str {
        "screen"
    }
    #[allow(clippy::must_use_candidate)]
    fn to_slug(&self) -> String {
        self.to_string().to_lowercase()
    }
    #[allow(clippy::must_use_candidate)]
    fn to_url(&self) -> String
    where
        Self: Default,
    {
        if *self == Self::default() {
            "/".to_string()
        } else {
            format!(
                "/?{}={}",
                Self::screen_param(),
                urlencoding::encode(&self.to_slug())
            )
        }
    }
    #[allow(clippy::must_use_candidate)]
    fn from_url(url: &str) -> Option<Self>
    where
        Self: Default,
    {
        let query = url.split('?').nth(1).unwrap_or("");
        let params: std::collections::BTreeMap<String, String> =
            form_urlencoded::parse(query.as_bytes())
                .into_owned()
                .collect();
        let key = Self::screen_param();
        if let Some(screen) = params.get(key) {
            screen.parse().ok()
        } else if let Some(component) = params.get("component") {
            component.parse().ok()
        } else {
            None
        }
    }
}

impl<T> Routable for T where
    T: Display + FromStr + Clone + PartialEq + Eq + std::fmt::Debug + Send + Sync + 'static
{
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RouteKind {
    Page,
    Category,
    Modal,
    External,
}

pub trait RouteMetadata: Routable {
    fn label(&self, lang: Language) -> Cow<'static, str>;
    fn parent(&self) -> Option<Self>;
    fn children(&self) -> Vec<Self>;
    fn kind(&self) -> RouteKind {
        RouteKind::Page
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreadcrumbSegment<R> {
    pub name: String,
    pub route: R,
    pub is_last: bool,
}

pub fn breadcrumbs_for<R>(route: &R, lang: Language) -> Vec<BreadcrumbSegment<R>>
where
    R: RouteMetadata + Clone,
{
    let mut chain = Vec::new();
    let mut current = Some(route.clone());
    while let Some(r) = current {
        if r.kind() != RouteKind::Category {
            chain.push(BreadcrumbSegment {
                name: r.label(lang).into_owned(),
                route: r.clone(),
                is_last: false,
            });
        }
        current = r.parent();
    }
    chain.reverse();
    if let Some(last) = chain.last_mut() {
        last.is_last = true;
    }
    chain
}

#[cfg(feature = "router")]
pub mod router_impl {
    use super::Routable;
    use crate::nav::NavHistory;
    use std::marker::PhantomData;

    pub struct AppRouter<R: Routable, S> {
        current: R,
        history: NavHistory<R>,
        _s: PhantomData<S>,
    }

    impl<R, S> AppRouter<R, S>
    where
        R: Routable + Default,
        S: 'static,
    {
        #[allow(clippy::needless_pass_by_value)]
        pub fn new(_state: &mut S, default: R) -> Self {
            let current = {
                #[cfg(target_arch = "wasm32")]
                {
                    crate::platform::web::location_href()
                        .and_then(|u| R::from_url(&u))
                        .unwrap_or_else(|| default.clone())
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    default.clone()
                }
            };
            let history = NavHistory::new(current.clone());
            Self {
                current,
                history,
                _s: PhantomData,
            }
        }

        pub fn current(&self) -> &R {
            &self.current
        }

        pub fn history(&self) -> &NavHistory<R> {
            &self.history
        }

        pub fn history_mut(&mut self) -> &mut NavHistory<R> {
            &mut self.history
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn navigate(&mut self, _state: &mut S, route: R) {
            if route == self.current {
                return;
            }
            self.history.push(route.clone());
            self.current = route;
            history_push(&self.current);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn replace(&mut self, _state: &mut S, route: R) {
            self.history.replace(route.clone());
            self.current = route;
            history_replace(&self.current);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn go_back(&mut self, _state: &mut S) -> Option<&R> {
            if let Some(r) = self.history.go_back() {
                self.current = r.clone();
                history_push(&self.current);
                Some(r)
            } else {
                None
            }
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn go_forward(&mut self, _state: &mut S) -> Option<&R> {
            if let Some(r) = self.history.go_forward() {
                self.current = r.clone();
                history_push(&self.current);
                Some(r)
            } else {
                None
            }
        }

        pub fn sync_from_url(&mut self, url: &str) -> Option<R> {
            let route = R::from_url(url)?;
            if route == self.current {
                None
            } else {
                self.current = route.clone();
                self.history.sync(&route);
                Some(route)
            }
        }

        pub fn ui(&mut self, _ui: &mut egui::Ui, _state: &mut S) {
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(href) = crate::platform::web::location_href() {
                    self.sync_from_url(&href);
                }
            }
        }

        pub fn active_route(&self) -> Option<String> {
            Some(self.current.to_url())
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn history_push<R>(route: &R)
    where
        R: Routable + Default,
    {
        #[cfg(target_arch = "wasm32")]
        {
            let url = route.to_url();
            let _ = crate::platform::web::history_push(&url);
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = route;
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn history_replace<R>(route: &R)
    where
        R: Routable + Default,
    {
        #[cfg(target_arch = "wasm32")]
        {
            let url = route.to_url();
            let _ = crate::platform::web::history_replace(&url);
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = route;
        }
    }
}

#[cfg(not(feature = "router"))]
pub mod router_impl {
    use super::Routable;
    use crate::nav::NavHistory;
    use std::marker::PhantomData;

    pub struct AppRouter<R: Routable, S> {
        current: R,
        history: NavHistory<R>,
        _s: PhantomData<S>,
    }

    impl<R, S> AppRouter<R, S>
    where
        R: Routable + Default,
        S: 'static,
    {
        #[allow(clippy::needless_pass_by_value)]
        pub fn new(_state: &mut S, default: R) -> Self {
            let current = {
                #[cfg(target_arch = "wasm32")]
                {
                    crate::platform::web::location_href()
                        .and_then(|u| R::from_url(&u))
                        .unwrap_or_else(|| default.clone())
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    default.clone()
                }
            };
            let history = NavHistory::new(current.clone());
            Self {
                current,
                history,
                _s: PhantomData,
            }
        }

        pub fn current(&self) -> &R {
            &self.current
        }

        pub fn history(&self) -> &NavHistory<R> {
            &self.history
        }

        pub fn history_mut(&mut self) -> &mut NavHistory<R> {
            &mut self.history
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn navigate(&mut self, _state: &mut S, route: R) {
            if route == self.current {
                return;
            }
            self.history.push(route.clone());
            self.current = route;
            history_push(&self.current);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn replace(&mut self, _state: &mut S, route: R) {
            self.history.replace(route.clone());
            self.current = route;
            history_replace(&self.current);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn go_back(&mut self, _state: &mut S) -> Option<&R> {
            if let Some(r) = self.history.go_back() {
                self.current = r.clone();
                history_push(&self.current);
                Some(r)
            } else {
                None
            }
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn go_forward(&mut self, _state: &mut S) -> Option<&R> {
            if let Some(r) = self.history.go_forward() {
                self.current = r.clone();
                history_push(&self.current);
                Some(r)
            } else {
                None
            }
        }

        pub fn sync_from_url(&mut self, url: &str) -> Option<R> {
            let route = R::from_url(url)?;
            if route == self.current {
                None
            } else {
                self.current = route.clone();
                self.history.sync(&route);
                Some(route)
            }
        }

        pub fn ui(&mut self, _ui: &mut egui::Ui, _state: &mut S) {
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(href) = crate::platform::web::location_href() {
                    self.sync_from_url(&href);
                }
            }
        }

        pub fn active_route(&self) -> Option<String> {
            Some(self.current.to_url())
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn history_push<R>(route: &R)
    where
        R: Routable + Default,
    {
        #[cfg(target_arch = "wasm32")]
        {
            let url = route.to_url();
            let _ = crate::platform::web::history_push(&url);
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = route;
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn history_replace<R>(route: &R)
    where
        R: Routable + Default,
    {
        #[cfg(target_arch = "wasm32")]
        {
            let url = route.to_url();
            let _ = crate::platform::web::history_replace(&url);
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = route;
        }
    }
}

pub use router_impl::AppRouter;
pub use router_impl::{history_push, history_replace};
