#![allow(dead_code)]

use cryptonote::progress::Job;
use dioxus::core::ScopeId;
use dioxus::prelude::{rsx, Signal, VirtualDom};

pub fn with_runtime<R>(body: impl FnOnce() -> R) -> R {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    dom.rebuild_in_place();
    dom.in_runtime(body)
}

pub fn progress() -> Signal<Option<Job>> {
    Signal::new_in_scope(None, ScopeId(0))
}

pub fn block_on<R>(fut: impl std::future::Future<Output = R>) -> R {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("tokio runtime")
        .block_on(fut)
}
