#![allow(dead_code)]

use cryptonote::progress::Job;
use dioxus::core::ScopeId;
use dioxus::prelude::{rsx, Signal, VirtualDom};

pub fn fast_kdf() {
    if std::env::var("FUNCTORA_KDF_M_COST_KIB").is_err() {
        std::env::set_var("FUNCTORA_KDF_M_COST_KIB", "1024");
        std::env::set_var("FUNCTORA_KDF_T_COST", "1");
    }
}

pub fn with_runtime<R>(body: impl FnOnce() -> R) -> R {
    fast_kdf();
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    dom.rebuild_in_place();
    dom.in_runtime(body)
}

pub fn progress() -> Signal<Option<Job>> {
    fast_kdf();
    Signal::new_in_scope(None, ScopeId(0))
}

pub fn block_on<R>(fut: impl std::future::Future<Output = R>) -> R {
    fast_kdf();
    tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("tokio runtime")
        .block_on(fut)
}
