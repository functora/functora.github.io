use core::marker::PhantomData;
use dioxus::prelude::*;
use dioxus::router::Navigator;

#[derive(Clone)]
pub struct Nav<R, I = WriteSignal<u32>> {
    navigator: Navigator,
    idx: I,
    _phantom: PhantomData<R>,
}

impl<R, I: PartialEq> PartialEq for Nav<R, I> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<R, I: Writable<Target = u32>> Nav<R, I> {
    pub fn new(navigator: Navigator, idx: I) -> Self {
        Self {
            navigator,
            idx,
            _phantom: PhantomData,
        }
    }

    pub fn go_back(&mut self) {
        if self.navigator.can_go_back() {
            self.idx.with_mut(|v| *v = v.saturating_sub(1));
            self.navigator.go_back();
        } else {
            self.idx.set(0);
        }
    }

    pub fn can_go_back(&self) -> bool {
        self.navigator.can_go_back()
    }

    pub fn has_navigated(&self) -> bool {
        self.idx.with(|v| *v > 0)
    }

    pub fn reset(&mut self) {
        self.idx.set(0);
    }

    pub fn increment(&mut self) {
        self.idx.with_mut(|v| *v = v.saturating_add(1));
    }

    pub fn decrement(&mut self) {
        self.idx.with_mut(|v| *v = v.saturating_sub(1));
    }
}

impl<R: Routable + Default + PartialEq, I: Writable<Target = u32>> Nav<R, I> {
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

pub fn use_nav<R: Routable, I: Writable<Target = u32>>(idx: I) -> Nav<R, I> {
    Nav::new(use_navigator(), idx)
}
