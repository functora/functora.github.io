use std::fmt::Display;
use std::str::FromStr;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreadcrumbSegment<R> {
    pub name: String,
    pub route: R,
    pub is_last: bool,
}

pub fn breadcrumbs_for<R, F>(route: &R, mut category_lookup: F) -> Vec<BreadcrumbSegment<R>>
where
    R: Routable + Clone,
    F: FnMut(&R) -> Option<(&'static str, &'static str)>,
{
    let Some((cat, comp)) = category_lookup(route) else {
        return Vec::new();
    };
    if cat.is_empty() {
        return vec![BreadcrumbSegment {
            name: comp.to_string(),
            route: route.clone(),
            is_last: true,
        }];
    }
    vec![
        BreadcrumbSegment {
            name: cat.to_string(),
            route: route.clone(),
            is_last: false,
        },
        BreadcrumbSegment {
            name: comp.to_string(),
            route: route.clone(),
            is_last: true,
        },
    ]
}

#[cfg(feature = "router")]
pub mod router_impl {
    use super::Routable;
    use std::marker::PhantomData;

    pub struct AppRouter<R: Routable, S> {
        current: R,
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
            Self {
                current,
                _s: PhantomData,
            }
        }

        pub fn current(&self) -> &R {
            &self.current
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn navigate(&mut self, _state: &mut S, route: R) {
            self.current = route.clone();
            history_push(&route);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn replace(&mut self, _state: &mut S, route: R) {
            self.current = route.clone();
            history_replace(&route);
        }

        pub fn sync_from_url(&mut self, url: &str) -> Option<R> {
            let route = R::from_url(url)?;
            if route != self.current {
                self.current = route.clone();
                return Some(route);
            }
            None
        }

        pub fn ui(&mut self, _ui: &mut egui::Ui, _state: &mut S) {
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(href) = crate::platform::web::location_href() {
                    if let Some(route) = R::from_url(&href) {
                        self.current = route;
                    } else if href.split('?').next().unwrap_or("").ends_with('/') || href == "/" {
                        self.current = R::default();
                    }
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
    use std::marker::PhantomData;

    pub struct AppRouter<R: Routable, S> {
        current: R,
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
            Self {
                current,
                _s: PhantomData,
            }
        }

        pub fn current(&self) -> &R {
            &self.current
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn navigate(&mut self, _state: &mut S, route: R) {
            self.current = route.clone();
            history_push(&route);
        }

        #[allow(clippy::needless_pass_by_value)]
        pub fn replace(&mut self, _state: &mut S, route: R) {
            self.current = route.clone();
            history_replace(&route);
        }

        pub fn sync_from_url(&mut self, url: &str) -> Option<R> {
            let route = R::from_url(url)?;
            if route != self.current {
                self.current = route.clone();
                return Some(route);
            }
            None
        }

        pub fn ui(&mut self, _ui: &mut egui::Ui, _state: &mut S) {
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(href) = crate::platform::web::location_href() {
                    if let Some(route) = R::from_url(&href) {
                        self.current = route;
                    }
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
