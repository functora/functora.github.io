use dioxus::prelude::*;
use std::sync::atomic::{AtomicBool, Ordering};

#[test]
fn nav_write_signal_conversion() {
    static EQUAL: AtomicBool = AtomicBool::new(false);
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<u32> = Signal::new(0);
        let write: WriteSignal<u32> = signal.into();
        EQUAL.store(write() == 0, Ordering::SeqCst);
        rsx! { "" }
    });
    dom.rebuild_in_place();
    assert!(EQUAL.load(Ordering::SeqCst));
}

#[test]
fn write_clipboard_message_conversion() {
    static NONE: AtomicBool = AtomicBool::new(false);
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<Option<String>> = Signal::new(None);
        let write: WriteSignal<Option<String>> = signal.into();
        NONE.store(write().is_none(), Ordering::SeqCst);
        rsx! { "" }
    });
    dom.rebuild_in_place();
    assert!(NONE.load(Ordering::SeqCst));
}
