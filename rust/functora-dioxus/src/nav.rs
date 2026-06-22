use core::marker::PhantomData;
use dioxus::prelude::*;
use dioxus::router::Navigator;

#[derive(Clone, Copy)]
pub struct Nav<R> {
    navigator: Navigator,
    idx: WriteSignal<u32>,
    _phantom: PhantomData<R>,
}

impl<R> PartialEq for Nav<R> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<R> Nav<R> {
    pub fn new(navigator: Navigator, idx: WriteSignal<u32>) -> Self {
        Self {
            navigator,
            idx,
            _phantom: PhantomData,
        }
    }

    pub fn go_back(&mut self) {
        if self.navigator.can_go_back() {
            self.idx.set((self.idx)().saturating_sub(1));
            self.navigator.go_back();
        } else {
            self.idx.set(0);
        }
    }

    pub fn can_go_back(&self) -> bool {
        self.navigator.can_go_back()
    }

    pub fn has_navigated(&self) -> bool {
        (self.idx)() > 0
    }

    pub fn reset(&mut self) {
        self.idx.set(0);
    }

    pub fn increment(&mut self) {
        self.idx.set((self.idx)().saturating_add(1));
    }

    pub fn decrement(&mut self) {
        self.idx.set((self.idx)().saturating_sub(1));
    }
}

impl<R: Routable + Default + PartialEq> Nav<R> {
    pub fn push(&mut self, route: R) {
        if route == R::default() {
            self.reset();
        } else {
            self.increment();
        }
        let _ = self.navigator.push(route);
    }

    pub fn push_route(&mut self, href: &str) {
        if let Ok(route) = href.parse::<R>() {
            self.push(route);
        }
    }
}

pub fn use_nav<R: Routable>(idx: WriteSignal<u32>) -> Nav<R> {
    Nav::new(use_navigator(), idx)
}
